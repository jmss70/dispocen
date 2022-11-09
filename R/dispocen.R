# Copyright © 2020 Universidad de Málaga
#
# This file is part of DispoCen.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of theW License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


#' Zipf law
zipf.law <- function(w, k=1,a=1,d=0) {
  f = k/(seq_along(w)+d)^a
  names(f) <- w
  f
}

#' Exponential law
exponential.law <- function(w,a=2,k=1,d=0) {
  f = k/a^(seq_along(w)+d)
  names(f) <- w
  f
}


#' Additive reduce law
additive.reduce.law <- function(w,k=1) {
  1 - prod(1-w)^k
}

#' Read dispocen file
#'
#' This function reads file in the expected format from dispocen
#' @param file File name of the file with the data
#' @keywords read, dispocen
read.dispocen <- function(file) {
  f = readLines(file)
  infos   = character()
  users   = character()
  centers = character()
  words   = list()

  for (i in seq_along(f)) {
    a <- str_match(f[i], "^(\\d+) (\\d+) (\\d+) (.+)$")[-1]
    infos[length(infos)+1] = a[1]
    users[length(users)+1] = a[2]
    centers[length(centers)+1] = a[3]
    w = strsplit(a[4], ",")[[1]]
    for (i in seq_along(w)) {
      w[i] = trimws(w[i])
    }
    words[[length(words)+1]] = w
  }
  d <- data.frame(infos=infos, users=users, centers=centers, words=I(words))
  d
}

# General function for obtaining the spectrum of a interest center
# data   -> data.frame as provided by read.dispocen
# law    -> law to quantify compatibility with interest center
# reduce -> law to reduce the valorations of each word in each center to just one number
# return a data.frame with the field availability added.
build.availability <- function(data,law=exponential.law, reduce=additive.reduce.law) {
  words        = character()
  availability = numeric()

  for (i in seq_along(data$words)) {
    words        = c(words, data$words[i][[1]])
    availability = c(availability, law(data$words[i][[1]]))
  }
  centers     = rep(data$centers, vapply(data$words, FUN = function(x) length(x), FUN.VALUE = 0L))

  new.data <- data.frame(centers=centers, words=words, availability=availability) %>%
    group_by(centers,words) %>%
    summarise(availability=reduce(availability),freq.abs=length(words),.groups="drop")
  total_by_center <- new.data %>%
    group_by(centers) %>%
    summarise(counttotal=sum(freq.abs), .groups="drop") %>% ungroup()
  new.data <-
    left_join(new.data, total_by_center, by="centers") %>%
    mutate(freq.rel=freq.abs/counttotal) %>%
    arrange(-availability,words) %>%
    group_by(centers) %>%
    mutate(freq.abs.cum = cumsum(freq.abs), freq.rel.cum = cumsum(freq.rel)) %>%
    ungroup() %>%
    arrange(centers,-availability)
  new.data$order <- unlist(tapply(new.data$words, new.data$centers,function(x) {seq_along(x)}))
  new.data %>% select(centers,words,order,availability,freq.abs,freq.rel,freq.abs.cum,freq.rel.cum)
}

# Obtain the availability following the López-Strassburger model
# return a data.frame with the field availability added.
build.lopezstrass.availability <- function(data) {
  # Máxima posición alcanzada
  n <- max(vapply(data$words, function(x) {length(x)}, FUN.VALUE=1L))
  # Número total de hablantes
  N <- length(unique(data$users))

  build.availability(data   = data,
                     law    = function(w) {exponential.law(w,a=exp(2.3/(n-1)),d=-1)},
                     reduce = function(x) {sum(x) / N})
}

# Obtain the availability following the Ávila-Sánchez model
# returns a data.frame with the field availability added.
build.avilasanchez.availability <- function(data, k=1) {
  build.availability(data,
                     law=function(x) {zipf.law(x,k=k,d=1)},
                     reduce=additive.reduce.law)
}

# Sugeno integral of the fuzzy set d, respect fuzzy measure g
# Returns the value of the integral

fuzzy.expected.value <- function(d, g=function(x) {length(x)/length(d)}, h=function(x) {sqrt(x)}, level=0) {
  ng <- function(x) {
    res <- g(x)
    for (i in seq(length.out=level)) {
      res = h(res)
    }
    res
  }

  # Calculate by function, and the levels for alpha
  levels <- sort(unique(d), decreasing=TRUE)
  # Determine alpha cuts and its measure
  gs <- sapply(levels,function(x) {ng(d[d >= x])})
  res  <- cbind(levels,gs)
  res <- max(apply(res,1,min))
  res
}

# Label each word in each interest center in order to build levels of centrality
# data -> Availability data.frame as returned by build.avilability
# levels -> numbers of levels to clasify vocabulary
# returns -> a new data.frame, similar to input, but with labeled levels
classify.availability.levels <- function(data, levels=6) {
  #new.data <- data %>% select(centers,words,order,availability)
  new.data <- data
  new.data$cutlevel = 0
  new.data$level = 0
  for (i in seq(length.out = levels)) {
    cuts <- tapply(new.data$availability,new.data$centers,function(x) {fuzzy.expected.value(x,level=i)})
    cuts <- data.frame(cuts=cuts,centers=names(cuts))
    new.data <- left_join(new.data, cuts, by="centers")
    new.data$level[new.data$availability >= new.data$cuts] <- i
    new.data$cutlevel[new.data$availability >= new.data$cuts] <- new.data$cuts[new.data$availability >= new.data$cuts]
    new.data <- new.data %>% select(centers,words,order,availability,level,cutlevel,freq.abs,freq.rel,freq.abs.cum,freq.rel.cum)
  }
  new.data %>% arrange(-availability,centers)
}

# Reduce data.frame to a
build.availability.levels <- function(data, showcutlevels=FALSE) {
  #mt <- tapply(data$words, list(data$centers,data$level),c,simplify=FALSE)
  mt <- tapply(data$words, list(data$centers,data$level),paste0,simplify=FALSE)
  dmt <- data.frame(mt)
  names(dmt) <- colnames(mt)
  dmt$centers <- rownames(dmt)
  dmt <- dmt %>%
    pivot_longer(-centers,names_to="level", values_to="words") %>%
    mutate(level=strtoi(level))
  dmt$count <- lapply(dmt$words, function(x) {length(unlist(x))}) %>% unlist
  dmt$words <- lapply(dmt$words, function(x){paste(x,collapse=", ")}) %>% unlist
  dmt <- dmt %>%
    select(centers,level,count,words)
  if (showcutlevels) {
    merge(dmt,
          data %>% select(centers,level,cutlevel) %>% unique(),
          by=c("centers", "level")) %>%
      arrange(centers,-level)
  } else {
    dmt %>%
      arrange(centers,-level)
  }
}


runUtility <- function() {
  appDir <- system.file("utility", package="dispocen")
  if (appDir=="") {
    stop("Could not find utility directory. Try re-installing `dispocen`", .call=FALSE)
  }
  shiny::runApp(appDir, display.mode="normal")
}

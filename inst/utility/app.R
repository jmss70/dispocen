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

library(shiny)
library(dispocen)
library(tidyverse)

models = data.frame(name=c("Lopez-Strassburger",
                           "Avila-Sanchez (Zipf + Additive)"),
                    funct=c('build.lopezstrass.availability',
                               'build.avilasanchez.availability'))

generateInform <- function(file, model) {
  data <- read.dispocen(file$datapath)
  centers <- data %>% select(centers) %>% arrange(centers) %>% unique() %>% pull()
  model <- models %>% filter(name==model) %>% select(funct) %>% pull()
  content <-
    c('---',
      'title: "Proyecto dispocen"',
      'output:',
      '  word_document: default',
      '  html_notebook: default',
      '  html_document:',
      '    df_print: paged',
      '  pdf_document: default',
      'always_allow_html: yes',
  '---',
  '*Store this file in the same directory where your data is stored')

  content <- c(content,
               '# Carga de librerías',
               '```{r message = FALSE, warning = FALSE}',
               'library(dispocen)',
               '```')
  content <- c(content,
               '# Carga de datos',
               '```{r}',
               paste('data <- read.dispocen("', file$name,'")', sep="", collapse=""),
               'data %>% ',
               '  head() %>% ',
               '  flextable() %>% autofit() %>% ',
               '  theme_booktabs()',
               '```',
               '')
  content <- c(content,
               '# Modelo de disponibilidad',
               '```{r}',
               paste('disponibilidad <- ', model, '(data)', sep="", collapse=""),
               'disponibilidad %>% ',
               '  head() %>% ',
               '  flextable() %>% ',
               '  set_header_labels(centers = "Centro de interés", ',
               '                    words="Palabra", ',
               '                    order="Orden", ',
               '                    availability="Disponibilidad", ',
               '                    freq.abs="Frecuencia absoluta", ',
               '                    freq.abs.cum="Frecuencia absoluta acumulada", ',
               '                    freq.rel="Frecuencia relativa", ',
               '                    freq.rel.cum="Frecuencia relativa acumulada")  %>% ',
               '  colformat_num(j=c(4,6,8), digits=6)  %>% ',
               '  width(j=c(1,3,5,7),width=.65)  %>% ',
               '  width(j=c(2,4,6,8),width=.9) %>% ',
               '  theme_booktabs()',
               '```',
               '')
  content <- c(content,
               "## Centros de interés")

  for (center in centers) {
    content <- c(content,
                 paste(c('### Centro de interés: ', center), sep="", collapse=""),
                 '```{r eval=FALSE}',
                 'disponibilidad %>% ',
                 paste(c('  filter(centers=="', center, '") %>% '), sep="", collapse=""),
                 '  arrange(-availability) %>% ',
                 '  flextable() %>% ',
                 '  set_header_labels(centers = "Centro de interés", ',
                 '                    words="Palabra", ',
                 '                    order="Orden", ',
                 '                    availability="Disponibilidad", ',
                 '                    freq.abs="Frecuencia absoluta", ',
                 '                    freq.abs.cum="Frecuencia absoluta acumulada", ',
                 '                    freq.rel="Frecuencia relativa", ',
                 '                    freq.rel.cum="Frecuencia relativa acumulada")  %>% ',
                 '  colformat_num(j=c(4,6,8), digits=6)  %>% ',
                 '  width(j=c(1,3,5,7),width=.65)  %>% ',
                 '  width(j=c(2,4,6,8),width=.9) %>% ',
                 '  theme_booktabs()',
                 '```',
                '',
                '```{r}',
                'disponibilidad %>% ',
                paste(c('  filter(centers=="', center, '") %>% '), sep="", collapse=""),
                '  arrange(-availability) %>% ',
                '  ggplot(aes(x=order, y=availability)) + geom_line() +',
                '  xlab("Sucesión de palabras") + ylab("Disponibilidad")',
                '```',
                '')
  }

  content = c(content,
              '## Visión general de los centros de interés',
              '```{r}',
              'disponibilidad %>%',
              '  arrange(-availability) %>% ',
              '  ggplot(aes(x=order,y=availability)) + geom_line() + facet_wrap(~centers)  +',
              '  xlab("Secuencia de palabras (por grado descendente de compatibilidad)") + ',
              '  ylab("Disponibilidad")',
              '```',
              '')
  content = c(content,
              '# Grupos de compatiblidad',
              '```{r}',
              'levels <- classify.availability.levels(disponibilidad)',
              '```',
              '```{r}',
              'clasificacion <- build.availability.levels(levels)',
              '```',
              '')
  content <- c(content,
               "## Centros de interés")

  for (center in centers) {
    content <- c(content,
                 paste(c('### Centro de interés: ', center), sep="", collapse=""),
                 '```{r eval=FALSE}',
                 'levels %>% ',
                 '   arrange(-availability) %>%',
                 '   select(-order) %>%',
                 paste(c('  filter(centers=="', center, '") %>% '), sep="", collapse=""),
                 '   flextable() %>% ',
                 '   set_header_labels(centers = "Centro de interés", ',
                 '                     words="Palabra", ',
                 '                     availability="Disponibilidad",',
                 '                     level="Nivel de disponibilidad",',
                 '                     cutlevel="Nivel de corte",',
                 '                     freq.abs="Frecuencia absoluta", ',
                 '                     freq.abs.cum="Frecuencia absoluta acumulada", ',
                 '                     freq.rel="Frecuencia relativa", ',
                 '                     freq.rel.cum="Frecuencia relativa acumulada") %>% ',
                 '   colformat_num(j=c(5,7,9), digits=5)  %>% ',
                 '   width(j=c(1,4,6,8),width=.65)  %>% ',
                 '   width(j=c(2,3,5,6,7,9),width=.75)  %>% ',
                 '   theme_booktabs()',
                 '```',
                 ' ',
                 '```{r}',
                 'levels %>%',
                 paste(c('  filter(centers=="', center, '") %>% '), sep="", collapse=""),
                 '  mutate(level=factor(level)) %>% ',
                 '  arrange(-availability) %>% ',
                 '  ggplot(aes(x=order,y=availability,color=level)) + geom_line() +',
                 '  xlab("Posición del término en el centro de interés") +',
                 '  ylab("Disponibilidad")',
                 '```',
                 '',
                 '```{r}',
                 'clasificacion %>%',
                 '  filter(level > 0) %>%',
                 paste(c('  filter(centers=="', center, '") %>% '), sep="", collapse=""),
                 '  flextable() %>% ',
                 '  set_header_labels(centers = "Centro de interés", ',
                 '                    words="Palabras",  ',
                 '                    level="Nivel de disponibilidad", ',
                 '                    count="Recuento") %>% ',
                 '  width(j=c(1,2,3),width=.75) %>% ',
                 '  width(j=4,width=4) %>% ',
                 '  align(j=4,align="right") %>% ',
                 '  theme_booktabs() ',
                 '```',
                 '```{r eval=FALSE}',
                 'clasificacion %>% ',
                 '  filter(level == 0) %>%',
                 paste(c('  filter(centers=="', center, '") %>% '), sep="", collapse=""),
                 '  flextable() %>% ',
                 '  set_header_labels(centers = "Centro de interés", ',
                 '                    words="Palabras",  ',
                 '                    level="Nivel de disponibilidad", ',
                 '                    count="Recuento") %>% ',
                 '  width(j=c(1,2,3),width=.75) %>% ',
                 '  width(j=4,width=4) %>% ',
                 '  align(j=4,align="right") %>% ',
                 '  theme_booktabs() ',
                 '```')
  }

  paste(content, sep="", collapse="\n")

}

ui <- fluidPage(
  titlePanel(h1("dispocen user interface")),
  mainPanel(h2("File to process"),
            fileInput("file",
                      "Choose file to load"),
            h2("Model to apply"),
            radioButtons("model", "Model to apply",
                         choices = list("Lopez-Strassburger",
                                        "Avila-Sanchez (Zipf + Additive)")),
            h2(""),
            downloadButton("report", "Generate report"))
)

server <- function(input, output, session) {
  output$report <- downloadHandler(
    filename="report.rmd",
    content = function(file) {
      content = generateInform(input$file, input$model)
      write(content, file=file)
      stopApp()
    }
  )
}

shinyApp(ui = ui, server = server)

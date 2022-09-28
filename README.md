# dispocen

This is the main respository of the Dispocen r-package, a library of functions focused on the lexicographic availability analysis by interest centers.

Dispocen is distributed under GPL version 3 (or we tried to do it).

Software requeriments:

* https://cloud.r-project.org/
* https://rstudio.com/products/rstudio/download/
* https://cran.r-project.org/bin/windows/Rtools/

It's recommended to upgrade all the packages previously to install any new package. So, execute:

    update.packages(ask=FALSE)
   
In order to install it, execute in a R prompt:

    install.packages("devtools")
    library(devtools)
    install_github("jmss70/dispocen")

# Remark


In R 4.0.4 we have found neccessary, in order to load devtools, install previously cachem. So,  the order to install is:

    install.packages("devtools")
    install.packages("cachem")
    library(devtools)
    install_github("jmss70/dispocen")

In R 4.0.5 we have found neccesary, in order to load devtools, install previously testthat. So, the order to install is:

    install.packages("testthat")
    install.packages("cachem")
    install.packages("devtools")
    library(devtools)
    install_github("jmss70/dispocen")

# Introductory video
https://www.youtube.com/watch?v=IU5VfUvG4Ag

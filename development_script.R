# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")

#' Load these packages in first
library(tidyverse)
library(roxygen2)
library(devtools)

roxygen2::roxygenise()
#' Run these three functions to build up the package
build()
install()
document()

devtools::build_manual()
devtools::check(manual=TRUE) # check whether everything is there

# install("../surveytools")
# install("../textworks")
library(surveytools)




#### Create PDF Manual ####
pack <- "surveytools"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(path)))

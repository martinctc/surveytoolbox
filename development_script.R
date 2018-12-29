install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")

library(tidyverse)
library(roxygen2)
library(devtools)

document()

install("../surveytools")
library(surveytools)

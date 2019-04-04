# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# install.packages("rstudioapi")
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

rstudioapi::isAvailable("0.99.149")

#' Load these packages in first
library(tidyverse)
library(devtools)
library(roxygen2)

roxygen2::roxygenise()
#' Run these three functions to build up the package
devtools::build()
devtools::install()
devtools::document()

devtools::build_manual()
devtools::check(manual=TRUE) # check whether everything is there
devtools::check()

# Run this to build and check in one line
devtools::check(document = TRUE, manual=TRUE)

# Good Practice
goodpractice::gp()

#--------------------------------------
#' Set up package to use testthat

devtools::use_testthat()
devtools::test()

#--------------------------------------

# install("../surveytools")
# install("../textworks")
library(surveytools)


#### Create PDF Manual ####
pack <- "surveytools"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(path)))

#### Create HEX Sticker ####
library(hexSticker)
sticker(expression(plot(cars, cex=.5, cex.axis=.5, mgp=c(0,.3,0), xlab="", ylab="")),
        package="hexSticker", p_size=8, s_x=1, s_y=.8, s_width=1.2, s_height=1,
        filename="baseplot.svg")
dev.off()

sticker("https://image.flaticon.com/icons/svg/1422/1422254.svg",
        package="surveytoolbox",
        p_size = 5,
        dpi = 600,
        filename="baseplot.svg")
dev.off()

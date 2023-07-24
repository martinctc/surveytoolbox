#' @title 
#' Calculate percentage impact from coefficients of a log-linear model
#' 
#' @description
#' This function exponentiates coefficients and takes out 1 to calculate the
#' percentage impact of each variable on the response variable in a log-linear
#' model. The function returns a tibble with three columns: `var`, `coef`, and
#' `pc_impact`.
#' 
#' @param x A log-linear model object.
#' 
#' @return A tibble with three columns: `var`, `coef`, and `pc_impact`.
#' 
#' @import dplyr
#' 
#' @param x Log-linear model to be passed through
#' 
#' @return a [tibble][tibble::tibble-package]    
#' 
#' @export
calc_pc_loglin <- function(x){
  x$coefficients %>%
    enframe(name = "var", value = "coef") %>%
    mutate(pc_impact = exp(coef)-1)
}
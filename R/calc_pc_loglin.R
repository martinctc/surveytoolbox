#' Calculate percentage impact from coefficients of a log-linear model
#' 
#' Exponentiates coefficients and takes out 1 to calculate percentage impact.
#' Returns a tibble
#' 
#' @import dplyr
#' 
#' @param x Log-linear model to be passed through
#' 
#' @export
calc_pc_loglin <- function(x){
  x$coefficients %>%
    enframe(name = "var", value = "coef") %>%
    mutate(pc_impact = exp(coef)-1)
}
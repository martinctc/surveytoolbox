#' Set variable labels
#' Pipe-workflow optimised
#' 
#' @param x Variable to assign variable labels to
#' @param variable_label String vector to be assigned as the variable label
#' @examples 
#' library(tibble)
#' library(dplyr)
#' library(magrittr)
#' tibble(RESPID=1:1000,
#'        Q1=sample(c(0,1,2),1000,replace=TRUE),
#'        Q2=sample(c(0,1),1000,replace=TRUE))-> df
#' df %>%
#'   mutate_at("Q1",funs(set_varl(.,"Which of the following groups do you fall into?"))) %>%
#'   .$Q1
#' @export
set_varl <- function(x,variable_label){
  attr(x,'label') <- variable_label
  x
}
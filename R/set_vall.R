#' @title Set value labels
#' 
#' @description 
#' A pipe-workflow optimised method to set value labels.
#' This is a wrapper around `haven::labelled()`, but preserves
#' the original variable label of the vector.
#' 
#' @param x Variable to assign value labels to
#' @param value_labels Named character vector to be assigned as value labels
#' 
#' @family Value labels
#' @family Labels
#' 
#' @return The input variable `x` as a labelled vector with value labels set.
#' 
#' @examples 
#' 
#' library(magrittr)
#' library(dplyr)
#' library(tibble)
#' tibble(RESPID = 1:1000,
#'        Q1 = sample(c(0, 1, 2), 1000, replace = TRUE),
#'        Q2 = sample(c(0, 1), 1000, replace = TRUE)) -> df
#' df %>%
#'   mutate(Q2 = set_varl(Q2, "What is your answer to this yes/no question?")) %>%
#'   mutate(Q2 = set_vall(Q2, c("No" = 0, "Yes" = 1))) %>%
#'   .$Q2 %>% attributes()
#' @export
set_vall <- function(x, value_labels) {
  label <- attr(x, 'label')
  x <- haven::labelled(x, value_labels)
  x <- set_varl(x, label)
  return(x)
}      

#' Apply a function rowwise, selecting variables with dplyr::select() syntax
#'
#' @param x Data frame or tibble to pass through.
#' @param select_helpers Select variables using dplyr::select() syntax
#' @param FUN Function to be applied to selected columns
#' 
#' @examples
#' \dontrun{
#' library(tidyverse)
#' iris %>% mutate(Sepal_Sum = apply_row(.,select_helpers = contains("Sepal"), sum, na.rm = TRUE))
#' iris %>% mutate(Sepal_Sum = apply_row(.,c("Sepal.Length","Petal.Width"), function(x) sum(x > 1))) 
#' }

#' 
#' @export

apply_row <- function(x, select_helpers = everything(), FUN, ...){
  apply(select(x,select_helpers),1, FUN, ...)
}


#' Remove all columns which contain only NAs
#' Optimised for magrittr / dplyr pipes
#' @param df Data frame to be passed through
#' @export
remove_na_only <- function(df){
  Filter(function(x)!all(is.na(x)),df)
}
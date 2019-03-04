#' Remove all columns which contain only zeros
#' Optimised for magrittr / dplyr pipes
#' @param df Data frame to be passed through  
remove_zero_only <- function(df){
  Filter(function(x)!all(x==0),df)
}
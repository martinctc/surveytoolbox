#' Squish - function for combining values if they are identical
#' 
#' Use this with group_by() to get to a "higher" level. 
#' One person may have multiple web visits, where all visits would have the same age and gender.
#' Getting to the person-level data (each observation is a person) would require 'summarising'.
#' Throws up an error message if values differ. 
#' 
#' @examples
#' squish(c(1,2,1))
#' squish(c(1,1,1))
#' 
#' @export
squish <- function(x){
  if(dplyr::n_distinct(x) == 1){
    x[[1]] # return first value
  } else {
    stop("More than one unique value in each row!")
  }
}




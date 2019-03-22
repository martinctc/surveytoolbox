#' Returns a single-length vector if all values in vector are identical. 
#' 
#' This can be used with group_by() to transform data to a "higher" level with hierarchically structured data. 
#' For instance, one person may have multiple web visits, where all visits would have the same age and gender.
#' Getting to the person-level data (each observation is a person) would require 'summarising'.
#' squish() throws up an error message if values differ. 
#' 
#' @examples
#' \dontrun{
#' squish(c(1,2,1))
#' } 
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




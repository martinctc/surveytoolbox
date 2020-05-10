#' @title Function that returns TRUE/FALSE if value exists in x, but returns NA if x consists entirely of NAs
#' 
#' @description 
#' A more nuanced response is returned than the standard R method,
#' which does not return NAs if x is all NAs.
#' Has useful applications in understanding a set of categorical variables
#' belonging to a single question.
#' E.g. A question on brand usage across 10 product types to understand 'any' usage of a brand x.
#' 
#' @return A logical vector whether a value exists in x, and returns NA if x contains only NAs.
#' @param x Vector of values to test.
#' @param value Value to test whether it exists in x. NA is returned if none exists at all. 
#' @examples
#' any_x(c(1,0,1),1) # TRUE
#' any_x(c(1,NA,1),1) # TRUE
#' any_x(c(0,0,NA),1) # FALSE
#' any_x(c(NA,NA,NA),1) # NA
#' 
#' @export
any_x <- function(x, value){
  if(all(is.na(x))){
    return(NA)
  } else(any(x %in% value,na.rm=TRUE))
}

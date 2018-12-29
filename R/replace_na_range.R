#' Replace NAs randomly from a selected range with replacement
#' 
#' @param x Original numeric vector containing missing values. 
#' @param range Vector specifying the valid values to replace the missing values with
#' @examples 
#' replace_na_range(c(1,NA,2,3,1,NA,2),c(1,2,3)) 
#' 
#' @export
replace_na_range <- function(x,range){
  sum_na <- sum(is.na(x))
  rand <- sample(range,sum_na,replace=TRUE)
  x[is.na(x)] <- rand
  x
}
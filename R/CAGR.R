#' CAGR Calculator
#' 
#' Calculates CAGR 
#' http://www.investopedia.com/terms/c/cagr.asp
#' 
#' @export
CAGR <- function(value.beginning, value.ending, number.of.periods){
  
  ((value.ending / value.beginning) ^ (1 / number.of.periods)) - 1
  
}
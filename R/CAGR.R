#' @title Calculate CAGR
#' 
#' @description Compute the Compound Annual Growth Rate (CAGR).
#' @param value_begin The value at the start of the series.
#' @param value_end The value at the end of the series.
#' @param n_periods The number of periods to base the CAGR calculations on.
#' 
#' @seealso http://www.investopedia.com/terms/c/cagr.asp
#' 
#' @return numeric value
#' 
#' @export
CAGR <- function(value_begin, value_end, n_periods){
  
  ((value_end / value_begin) ^ (1 / n_periods)) - 1
  
}
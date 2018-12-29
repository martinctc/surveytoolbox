#' Calculate percentage change of a vector relative to a lag k
#'
#' @param x A vector to be passed through
#' @param lag The number of lags used, defaults to 1
#' @export
pc_change <-function(x,lag=1){
  base <- lag(x,lag) # base to be divided by
  c(rep(NA,lag),diff(x,lag))/base
}
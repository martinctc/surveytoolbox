#' Convert as percent (string)
#' Convert a numeric value into a string with percentage sign.
#' @param num Numeric vector to pass through
#' @param rounding Number of decimal places to round to. Default is 0.
#' @export
as_percent <- function(num, rounding = 0){
  paste0(round(num * 100, rounding),"%")
}

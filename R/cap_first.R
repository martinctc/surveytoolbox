#' Capitalise the first letter of all words
#' 
#' @param x String, or a character vector
#' @examples
#' cap_first("steeles pots and pans")
#' 
#' @export
cap_first <- function(string) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", string, perl=TRUE)
}     
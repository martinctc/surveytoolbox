#' @title Wrap text based on character threshold
#'
#' @description Wrap text in visualizations according to a preset character
#' threshold. The next space in the string is replaced with `\n`, which will
#' render as next line in plots and messages.
#'
#' @param x String to wrap text
#' @param threshold Numeric, defaults to 15. Number of character units by which
#' the next space would be replaced with `\n` to move text to next line.
#'
#' @examples
#' wrapped <- wrap_text(
#'   "The total entropy of an isolated system can never decrease."
#'   )
#' message(wrapped)
#'
#' @return
#' String output representing a processed version of `x`, with spaces replaced
#' by `\n.`
#'
#' @export
wrap_text <- function(x, threshold = 15){
  
  # No changes if string is shorter than threshold
  
  patt <- paste0(
    '(.{1,',
    threshold,
    '})(\\s|$)'
  )
  
  new_x <- gsub(
    pattern = patt,
    replacement = '\\1\n',
    x = x
    )
  
  ifelse(
    test = (nchar(x) <= threshold),
    yes = x, 
    no = new_x
  )
}
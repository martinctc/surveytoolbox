#' Reverse a Likert scale
#' 
#' Reverse a Likert scale such that on a 0-10 scale, 10 becomes 0 and 0 becomes 10. 
#' Use the `top` and `bottom` arguments to specify the valid range of the scale. 
#' 
#' @param x Numeric vector
#' @param top Top value of the scale for the variable, e.g. 10 for a 0-10 scale.
#' @param bottom value of the scale for the variable, e.g. 0 for a 0-10 scale.
#' @keywords likert scale
#' @return A numeric vector which is the "reverse" of the original likert scale variable.
#' @examples 
#' data <-c(5, 4, 3, 2, 6)
#' likert_reverse(data, 6, 0)
#' @export
likert_reverse <- function(x,top,bottom){
  total <-top+bottom
  x <- total-x
  x
}
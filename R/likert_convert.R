#' Convert a Likert scale from one scale to another
#' 
#' This is used for converting data, for instance, from a 6-point scale to a 5-point scale.
#' The scale "dimensions" are specified in the function argument, and the function converts the numeric vector that is passed through. 
#' 
#' @param x Numeric vector to be passed through.
#' @param top.x Top value of the original scale. This would be 6 on a 0-6 scale.
#' @param bot.x Bottom value of the original scale. This would be 0 on a 0-6 scale.
#' @param top.y Top value of the new/target scale. This would be 5 on a 0-5 scale.
#' @param bot.y Bottom value of the new/target scale. This would be 0 on a 0-5 scale.
#' @keywords likert scale
#' @return A numeric vector containing the rescaled likert scale variable.
#' @examples 
#' data <-c(5, 4, 3, 2, 1)
#'likert_convert(data,5,0,10,0) #5-point scale to 10-point scale
#'[1] 10  8  6  4  2
#'@export
likert_convert <- function(x, top.x, bot.x, top.y, bot.y){
  y <- ((top.y-bot.y)*(x-bot.x)/(top.x-bot.x))+bot.y
  y
}
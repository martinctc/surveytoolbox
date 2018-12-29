#' Replace nth occurring value in a string
#' 
#' A more refined replacement function than str_replace() and str_replace_all()
#' 
#' @param x Pass string character
#' @param pattern String containing characters to match
#' @param replacement String containing to replace
#' @param n Nth term to be replaced
#' @export
str_replace_nth <- function(x, pattern, replacement, n) {
  g <- gregexpr(pattern, x)[[1]][n]
  s <- scan(text = gsub("[()]", "", pattern),
            sep = "|",
            what = "")
  substr(x, g, g) <- replacement[match(substr(x, g, g), s)]
  x
}
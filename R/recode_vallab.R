#' Recode value labels based on numeric code
#' 
#' @param x haven_labelled vector to be passed through.
#' @param code Numeric value that specifies code where value label is to be replaced.
#' @param new_label Character string to be used as the new value label.
#' 
#' @examples 
#' x <- haven::labelled(c(1,2,3,2,1),c("a" = 1, "b" = 2, "d" = 3))
#' recode_vallab(x, code = 2, new_label = "Surprise!")
#' 
#' @export
recode_vallab <- function(x, code, new_label){
  attr(x,"labels") -> label_x
  
  names(label_x)[which(label_x == code)] <- new_label # assign new_label to names of attr(x,"labels")
  
  attr(x,"labels") <- label_x # assign label_x to 'labels' attr of original vector
  
  return(x)
}
#' Convert ordinal variables into binary variables by "boxing"
#' 
#' For instance, you can create a Top Two Box variable from a 7-point agreement scale question.
#' Function returns a labelled double variable, which will have value label attributes.
#' 
#' @param x Variable to be passed through
#' @param which Character string to specify which end of spectrum to take values - valid inputs are "top" and "bottom"
#' @param number Number to take values from
#' @examples
#' box_it(sample(1:10,100,replace=TRUE)) # Converted to binary variable where 9, 10 are selected
#' @export
box_it <-function(x,which="top",number=2){
  max_x <- max(x,na.rm = TRUE)
  min_x <- min(x,na.rm = TRUE)
  
  if(which=="top"){
    valid_range <- (max_x - number + 1):max_x
  } else if(which=="bottom"){
    valid_range <- min_x:(min_x + number - 1)
  }
  
  dplyr::case_when(x %in% valid_range~1,
                   !(x %in% valid_range)~0,
                   TRUE~NA_real_) -> output
  
  attr(output,'labels') <- c("Selected"=1,
                             "Not selected"=0,
                             "Missing value"=NA_real_)
  
  range_print <- paste0(valid_range,collapse=", ")
  message(paste0("Converted to binary variable where ",
                 range_print,
                 " are selected"))
  return(output)
} 
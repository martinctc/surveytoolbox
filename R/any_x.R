#' Function that returns TRUE/FALSE if value exists in x, but returns NA if x consists entirely of NAs
#' 
#' @examples
#' any_x(c(1,0,1),1) # TRUE
#' any_x(c(1,NA,1),1) # TRUE
#' any_x(c(0,0,NA),1) # FALSE
#' any_x(c(NA,NA,NA),1) # NA
#' 
#' @export
any_x <- function(x, value){
  if(all(is.na(x))){
    return(NA)
  } else(any(x %in% value,na.rm=TRUE))
}

#' Create file name with time stamp
#' 
#' This function generates a character string that suffixes a file name with a time stamp.
#' @param main The main file name to be used. 
#' @param extension The file extension to be used, e.g. ".csv"
#' @import stringr
#' @examples
#' timed_fn("Q15. ",".xlsx")
#' @export
timed_fn <- function(main,extension){
  Sys.time() %>%
    stringr::str_replace_all("[[:punct:]]","") %>%
    stringr::str_replace_all(" ","_") -> xtime
  
  paste(main,xtime,extension)
}
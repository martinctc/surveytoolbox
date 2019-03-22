#' Create a named list object with two vectors
#' 
#' Input two vectors of the same length and return a named list.
#' @param x Vector containing character strings, e.g. "A"
#' @param y Vector containing values for the list, e.g. 1, 3
#' @examples
#' create_named_list(c("Alice","Bob","Carol"),c(54,60,23))
#' @export
create_named_list <- function(x, y){
  quostr <- function(list){
    options(useFancyQuotes = FALSE)
    list2 <- lapply(list,function(x)toString(dQuote(x)))
    list2
  }
  
  weave <- function(list1, list2, num){
    paste0(list1[num],"=",list2[num])
  }
  
  len <- length(x)
  
  temp <- sapply(1:len,weave, list1=quostr(x),list2=y)
  temp <- paste(temp, collapse = ", ")
  temp <- paste0("c(",temp,")")
  return(eval(parse(text=temp)))
}

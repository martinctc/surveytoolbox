#' Append an item to a list dynamically (pipe-optimised)
#' 
#' Function directly appends item to list in Global Environment
#' @param x An object to append to list, e.g. vector, data frame. 
#' @param list_x Target list to append object to. 
#' @param name Specify a character string for the name of the list. Defaults to blank
#'
#' @examples
#' a_list <- list(NULL)
#' append_to_list(iris,a_list,"iris") 
#' @export
append_to_list <- function(x, list_x, name=""){
  
  temp <- deparse(substitute(list_x))
  
  list_len <- length(list_x)
  list_len1 <- list_len + 1
  
  if(is.null(list_x[[1]])){ # Treatment if list is empty
    list_x[[1]] <- x
    names(list_x)[1] <- name
    assign(temp,list_x,envir = .GlobalEnv)
  } else {  # Treatment if list has already got at least one object
    list_x[[list_len1]] <- x
    names(list_x)[list_len1] <- name
    assign(temp,list_x,envir = .GlobalEnv)
  }
}
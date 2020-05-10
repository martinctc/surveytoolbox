#' @title Append an item to a list dynamically
#' 
#' @description
#' The `append_to_list()` function appends an object to the specified list in Global Environment (default).
#' This function is pipe-optimised, and allows the option of specifying a name for the new object in the list.
#' 
#' @param x An object to append to list, e.g. vector, data frame. 
#' @param list_x Target list to append object to. 
#' @param name Specify a character string for the name of the list. Defaults to blank
#' @param enviro Specifies the environment
#'
#' @examples
#' a_list <- list(NULL)
#' append_to_list(iris,a_list,"iris") 
#' 
#' @export
append_to_list <- function(x, list_x, name="", enviro = .GlobalEnv){
  
  temp <- deparse(substitute(list_x))
  
  list_len <- length(list_x)
  list_len1 <- list_len + 1
  
  if(is.null(list_x[[1]])){ # Treatment if list is empty
    list_x[[1]] <- x
    names(list_x)[1] <- name
    assign(temp,list_x,envir = enviro)
  } else {  # Treatment if list has already got at least one object
    list_x[[list_len1]] <- x
    names(list_x)[list_len1] <- name
    assign(temp,list_x,envir = enviro)
  }
}

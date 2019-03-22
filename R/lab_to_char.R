#' Convert labelled double variable to character variable
#' 
#' Returns error if variable is not of class labelled double.
#' 
#' @param x Labelled double vector to pass through
#' @export
lab_to_char <- function(x){
  if(!haven::is.labelled(x)){
    stop("Variable is not a labelled double")
  } else if(haven::is.labelled(x)){
    names(attr(x,'labels')) -> id
    attr(x,'labels')-> var
    
    tibble::tibble(id=id,var=var)-> key_tab
    
    look_up(x,key_tab,column = 1)
  }
}  
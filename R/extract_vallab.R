#' Return value labels as tibble
#' Only works if variable as value label          
#' @param x Data frame or tibble containing the variable           
#' @param variable Enter variable name as string  
#' @export                   
extract_vallab <- function(x,variable){
  if(is.null(attr(x[[variable]],'labels'))){
    return (NULL) # Return NULL if no value labels found
  } else {
    x[[variable]] %>%
      attr('labels') %>%
      stack() %>%
      dplyr::as_tibble() %>%
      `names<-`(c("id",variable)) %>%
      mutate_at(vars(variable),funs(as.character))
  }
}
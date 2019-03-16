#' Return value labels as tibble
#' 
#' This function is conditional on the variable having value labels.
#' @param x Data frame or tibble containing the variable           
#' @param variable Enter variable name as string  
#' @export                   
extract_vallab <- function(x,variable){
  if(is.null(attr(x[[variable]],'labels'))){
    return (NULL) # Return NULL if no value labels found
  } else {
    x[[variable]] %>%
      attr('labels') %>%
      utils::stack() %>%
      dplyr::as_tibble() %>%
      `names<-`(c("id",variable)) %>%
      dplyr::mutate_at(vars(variable),~as.character(.))
  }
}
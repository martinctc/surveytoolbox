#' @title
#' Convert character variable to labelled integer variable
#' 
#' @description
#' This function converts the character values into value labels, assigning each
#' value an integer. To achieve the same effect whilst prescribing a set of
#' value-to-label mapping to the function, please see `char_to_var()`.
#' 
#' @param x Character vector to pass through
#' 
#' @importFrom tidyr drop_na
#' 
#' @export
char_to_lab <- function(x){
  
  unique_x <- unique(x)
  
  gen_df <- tibble::tibble(
    id = 1:length(unique_x),
    var = as.character(unique_x)
    )
  
  value_labels <- unlist(create_named_list(gen_df$var,gen_df$id))
  
  sapply(1:length(x),function(i){
    if(is.na(x[[i]])){
      NA
    } else {
      gen_df[gen_df$var==x[[i]],1] %>% tidyr::drop_na() %>% unlist()
    }
  }) -> outcome # Convert current values to assigned numbers
  
  return(set_vall(outcome,value_labels)) # Set value labels
}
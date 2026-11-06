#' @title
#' Convert character variable to labelled integer variable
#' 
#' @description
#' This function converts the character values into value labels, assigning each
#' value an integer. To achieve the same effect whilst prescribing a set of
#' value-to-label mappings to the function, please see `chr_to_var()`.
#' 
#' @param x Character vector to pass through
#' 
#' @importFrom tidyr drop_na
#' 
#' @export
char_to_lab <- function(x){
  
  unique_x <- unique(x[!is.na(x)])
  
  gen_df <- tibble::tibble(
    id = seq_along(unique_x),
    var = as.character(unique_x)
    )
  
  value_labels <- unlist(create_named_list(gen_df$var,gen_df$id))
  
  sapply(seq_along(x),function(i){
    if(is.na(x[[i]])){
      NA
    } else {
      gen_df[gen_df$var==x[[i]],1] %>% tidyr::drop_na() %>% unlist()
    }
  }) -> outcome # Convert current values to assigned numbers
  
  return(set_vall(outcome,value_labels)) # Set value labels
}

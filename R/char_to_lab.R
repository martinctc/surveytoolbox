#' Convert character variable to labelled integer variable
#' 
#' This function converts the character values into value labels, assigning each value an integer.
#' 
#' @param x Character vector to pass through
#' 
#' @export
char_to_lab <- function(x){
  unique_x <- unique(x)
  
  gen_df <- tibble::tibble(id=1:length(unique_x),
                           var=as.character(unique_x))
  
  value_labels <- unlist(create_named_list(gen_df$var,gen_df$id))
  
  sapply(1:length(x),function(i){
    if(is.na(x[[i]])){
      NA
    } else {
      gen_df[gen_df$var==x[[i]],1] %>% drop_na() %>% unlist()
    }
  }) -> outcome # Convert current values to assigned numbers
  
  return(set_vall(outcome,value_labels)) # Set value labels
}
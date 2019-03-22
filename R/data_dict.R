#' Create a Data Dictionary from a data frame with Variable and Value Labels.
#' 
#' This function mimics exported objects from SPSS.
#' A table is returned, summarising the variable and value labels that exist in the data frame.
#' 
#' @param x A data frame object to be passed through.
#' @param max_char Maximum character allowed on Variable and Value label columns.       
#' @export              
data_dict <-function(x,max_char=32760){
  suppressWarnings(
    lapply(1:ncol(x),
           function(i){
             namex <- names(x)[[i]]
             purrr::pluck(x,namex,purrr::attr_getter("label")) %>% as.character() -> label_var
             purrr::pluck(x,namex,purrr::attr_getter("labels")) %>%
               names() %>%
               paste(collapse="; ") %>%             
               stringr::str_trunc(max_char)-> label_val
             x[[namex]] %>%
               unique() %>%
               paste(collapse="; ") %>%
               stringr::str_trunc(max_char)-> val_val
             
             
             ifelse(is_null(label_var),"",label_var)->label_var
             ifelse(is_null(label_val),"",label_val)->label_val
             
             data.frame(var=namex,label_var,label_val,value=val_val)
           }) %>% dplyr::bind_rows()
  )
}
#' @title Create a Data Dictionary from a data frame with Variable and Value Labels.
#' 
#' @description
#' This function mimics exported objects from SPSS.
#' A table is returned, summarising the variable and value labels that exist in the data frame.
#' 
#' @param x A data frame object to be passed through.
#' @param max_char Maximum character allowed on Variable and Value label columns.
#' 
#' @return a [tibble][tibble::tibble-package]    
#' 
#' @export              
data_dict <-function(x, max_char=32760){
  suppressWarnings(
    lapply(1:ncol(x),
           function(i){
             namex <- names(x)[[i]] # Extract name of each column
             
             label_var <-
               purrr::pluck(x,namex,purrr::attr_getter("label")) %>%
               as.character()
             
             label_val <-
               purrr::pluck(x,namex,purrr::attr_getter("labels")) %>%
               names() %>%
               paste(collapse="; ") %>%             
               stringr::str_trunc(max_char)
             
             val_val <-
             x[[namex]] %>%
               unique() %>%
               sort() %>% # Fix random order bug
               paste(collapse="; ") %>%
               stringr::str_trunc(max_char)
             
             
             ifelse(is_null(label_var),"",label_var)->label_var
             ifelse(is_null(label_val),"",label_val)->label_val
             
             data.frame(var=namex,
                        label_var,
                        label_val,
                        value=val_val)
           }) %>% dplyr::bind_rows()
  )
}

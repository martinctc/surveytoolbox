#' Create a Data Dictionary from a data frame with Variable and Value Labels
#' Exported objects from SPSS
#' 
#' @param x data frame
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
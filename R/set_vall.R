#' Set value labels
#' Pipe-workflow optimised
#' 
#' 
#' @param x Variable to assign value labels to
#' @param value_labels Named character vector to be assigned as value labels
#' @examples 
#' 
#' library(magrittr)
#' library(dplyr)
#' library(tibble)
#' tibble(RESPID=1:1000,
#'        Q1=sample(c(0,1,2),1000,replace=TRUE),
#'        Q2=sample(c(0,1),1000,replace=TRUE))-> df
#' df %>%
#'   mutate_at("Q2",funs(set_varl(.,"What is your answer to this yes/no question?"))) %>%
#'   mutate_at("Q2",funs(set_vall(.,c("No"=0,"Yes"=1)))) %>%
#'   .$Q2 %>% attributes()
#' @export
set_vall <- function(x,value_labels){
  label <- attr(x, 'label')
  x <- haven::labelled(x,value_labels)
  x <- set_varl(x, label)
}      

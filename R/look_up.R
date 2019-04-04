#' Replace x values with corresponding values using a key
#' 
#' @param x A string vector to be matched.
#' @param table Lookup table
#' @param index Character string of the key/index column used for matching
#' @param column Column index (integer) or name (string) to return from the lookup table. 
#' Default is the second column.
#' @examples
#' library(magrittr)
#' library(dplyr)
#' iris %>% .$Petal.Width %>%
#'  look_up(tibble(var=0.2,return="zero-point-two"))
#' @export
look_up <- function(x,table,index="var",column=2){
  sapply(seq_along(1:length(x)),
         function(i){
           if(is.na(x[[i]])){
             return("")
           } else {
             unlist(table[table[[index]]==x[[i]],column])
           }
         }) %>% as.character() -> matched_x
  
  sapply(seq_along(1:length(x)),
         function(i){
           if(matched_x[[i]]=="character(0)"){
             x[[i]]
           } else {
             matched_x[[i]]
           }
         }) %>% ifelse(.=="",NA_character_,.)
}         

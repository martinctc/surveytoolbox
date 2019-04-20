look_up2 <- function(x,table,index="var",column=2){
  if(!is.character(table[[index]]) & !is.numeric(table[[index]])){
    stop("Invalid class: index column is neither a character or numeric vector. 
         Please convert class before running.")
  } 
  
  if(anyDuplicated(table[[index]]) > 1){
    stop("Error: duplicates in index column.
         Please remove duplicates before running.")
  }
  
  vapply(seq_along(1:length(x)),
         function(i){
           if(is.na(x[[i]])){
             return("")
           } else {
             table[table[[index]]==x[[i]],column] %>% unlist() %>% as.character()
           }
         }, FUN.VALUE = character(0)) -> matched_x
  
  sapply(seq_along(1:length(x)),
         function(i){
           if(matched_x[[i]]==""){
             x[[i]]
           } else {
             matched_x[[i]]
           }
         }) %>% ifelse(.=="",NA_character_,.)
}

library(tidyverse)
match_df <- data.frame(col1 = c("setosa","versicolor","virginica"),
                       col2 = toupper(c("setosa","versicolor","virginica")))

match_df2 <- data.frame(col1 = c("setosa","versicolor","virginica"),
                        col2 = toupper(c("setosa","versicolor","virginica")), stringsAsFactors = FALSE)

iris %>% mutate_at(vars(Species),~look_up2(.,match_df,index = "col1", column = "col2"))
iris %>% mutate_at(vars(Species),~look_up2(.,match_df2,index = "col1", column = "col2"))

#' Implemented class checking.
#' Forbid running function if index vector is not of numeric or character vector type.

iris %>%
  pull(Species) %>%
  look_up2(match_df2, index = "col1", column = "col2")

c("setosa",NA_character_,"daisy") %>%
  look_up2(match_df2,index = "col1", column = "col2")

c("setosa","versicolor","virginica") %>%
  look_up2(match_df2,index = "col1", column = "col2")

c("setosa","versicolor",NA_character_) %>%
  look_up2(match_df2,index = "col1", column = "col2")

1:3 %>%
  look_up2(match_df2,index = "col1", column = "col2")

c(5.7,5.2) %>%
  look_up2(iris,"Petal.Length","Petal.Width")

c(1,2,3, NA_integer_) %>%
  look_up2(tibble(x = 1:5,y = letters[1:5]),"x","y")

c("b","b",NA_character_) %>%
  look_up2(tibble(x = 1:5,y = letters[1:5]),"y","x")
  
x <- c("setosa","versicolor",NA_character_)

sapply(seq_along(1:length(x)),
       function(i){
         if(is.na(x[[i]])){
           return("")
         } else {
           match_df[match_df[["col1"]]==x[[i]],"col2"] %>% unlist()
         }
       })

#' Clean strings so that they can be used as variable names or column names
#' 
#' @param string Character string to be "cleaned".
#' @param treat_dups Set to FALSE to allow duplication of strings. Defaults to TRUE
#' @return A "cleaned" string character vector.
#' @importFrom magrittr %>%
#' @examples
#' clean_strings(c("Respondent ID","Q23. Brand Awareness"))
#' @export
clean_strings <- function(string,treat_dups=TRUE){
  new_string <- string %>%
    gsub("'", "", .) %>%
    gsub("\"","", .) %>%
    gsub("%", "percent", .) %>%
    gsub("^[ ]+","", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>% 
    gsub("[_]+", "_", .) %>%
    tolower(.) %>%
    gsub("_$", "",.)
  
  dupe_count <- vapply(1:length(new_string), function(i) {
    sum(new_string[i] == new_string[1:i])
  }, FUN.VALUE = integer(1))
  
  if(treat_dups==TRUE){
    new_string[dupe_count > 1] <- paste(new_string[dupe_count > 1],
                                        dupe_count[dupe_count > 1], sep = "_")
    return(new_string)
  } else if(treat_dups==FALSE){
    return(new_string)
  }
}

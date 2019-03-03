#' Function to create a loadings file from the factanal() output
#' 
#' @param fa_object factanal() model
#' @keywords factor analysis
#' @examples 
#' fa_output <- factanal(drop_na(psych::bfi), factors = 6)
#' extract_fa_loads(fa_output)
#' @export
extract_fa_loads <-function(fa_object){
  loadings_object <- as.matrix(fa_object$loadings)
  
  # Find max and return column header
  loadings_max <- colnames(loadings_object)[apply(loadings_object,1,which.max)] 
  
  cbind(loadings_object,loadings_max) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Variable") %>%
    dplyr::mutate_all(~as.character(.)) %>%
    readr::type_convert() %>%
    return()
}
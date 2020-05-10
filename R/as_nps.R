#' @title Convert numeric variable to NPS variable  
#' 
#' @description 
#' Convert a bounded numeric variable [0, 10] to a labelled vector where
#' 0 to 6 is a _Detractor_, 7 to 8 is a _Passive_, and 9 to 10 is a
#' _Promoter_. The mean of the returned variable gives the Net Promoter Score (NPS).
#' 
#' @param x Numeric variable to pass through. 
#' Valid range is 0 to 10 inclusive, otherwise returns an error message.
#' 
#' @export
as_nps <-function(x){
  
  complete_x <- x[!is.na(x)]
  
  if(any(!complete_x %in% 0:10)){
    stop("Values out of bounds for NPS. Please check inputs.")
  }
  
  dplyr::case_when(x >=0 & x <=6 ~ -100,
                   x >=7 & x <=8 ~ 0,
                   x >=9 & x <=10 ~ 100,
                   TRUE~NA_real_)-> out_x
  
  labz <- c("Detractor" = -100,
            "Passive" = 0,
            "Promoter" = 100,
            "Missing value" = NA_real_)
  
  labz_pure <- c("Detractor" = -100,
                 "Passive" = 0,
                 "Promoter" = 100)
  
  
  if(length(complete_x) == length(x)){
    
    return(haven::labelled(out_x,labz_pure))
    
  } else {
    
    return(haven::labelled(out_x,labz))
  }
}

#' Convert numeric variable to NPS categorical variable
#' 
#' Returns a categorical variable with default values over 1, 2, and 3.
#' Suited for running multinomial logistic regression. 
#' To calculate the NPS score, use `as_nps()`.
#' 
#' @param x Numeric variable to pass through. Valid range is 0 to 10 inclusive, otherwise returns a NA.
#' @param det Numeric value to represent the code for Detractor. Defaults to 1.
#' @param pas Numeric value to represent the code for Passive. Defaults to 2.
#' @param pro Numeric value to represent the code for Promoter. Defaults to 3.
#' 
#' @export
as_nps_cat <-function(x, det = 1, pas = 2, pro = 3){
  if(any(!is.numeric(c(det, pas, pro)))){
    stop("Values must be numeric.")
  } else {
    dplyr::case_when(x >=0 & x<=6 ~ det,
                     x >=7 & x<=8 ~ pas,
                     x >=9 & x<=10 ~ pro,
                     TRUE ~ NA_real_)-> out_x
    
    labz <- c("Detractor" = det,
              "Passive" = pas,
              "Promoter" = pro,
              "Missing value" = NA_real_)
    
    haven::labelled(out_x,labz)
  }
}
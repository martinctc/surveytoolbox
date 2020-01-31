#' Convert numeric variable to NPS categorical variable
#' 
#' Returns a categorical variable with values 1, 2, and 3.
#' Suited for running multinomial logistic regression. 
#' To calculate the NPS score, use `as_nps()`.
#' 
#' @param x Numeric variable to pass through. Valid range is 0 to 10 inclusive, otherwise returns a NA.
#' 
#' @export
as_nps_cat <-function(x){
  dplyr::case_when(x >=0 & x<=6 ~ 1,
                   x >=7 & x<=8 ~ 2,
                   x >=9 & x<=10 ~ 3,
                   TRUE ~ NA_real_)-> out_x
  
  labz <- c("Detractor" = 1,
            "Passive" = 2,
            "Promoter" = 3,
            "Missing value" = NA_real_)
  
  haven::labelled(out_x,labz)
}
#' Convert numeric variable to NPS variable  
#' 
#' @param x Numeric variable to pass through. Valid range is 0 to 10 inclusive, otherwise returns a NA.
#' 
#' @export
as_nps <-function(x){
  dplyr::case_when(x >=0 & x<=6~-100,
                   x >=7 & x<=8~0,
                   x >=9 & x<=10~100,
                   TRUE~NA_real_)-> out_x
  
  labz <- c("Detractor"=-100,
            "Passive"=0,
            "Promoter"=100,
            "Missing value"=NA_real_)
  
  haven::labelled(out_x,labz)
}
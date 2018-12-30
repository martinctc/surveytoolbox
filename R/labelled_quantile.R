#' Convert a numeric into a quantile categorical variable, labelled by lower and upper bounds of quantiles (string)
#' @param x Vector, column, or numeric variable to be passed through 
#' @examples
#' labelled_quantile(c(1,1,1,1,2,3,5,5,6))
#' [1] "1.2_TO_2.8" "1.2_TO_2.8" "1.2_TO_2.8" "1.2_TO_2.8" "2.8_TO_5"   "5_TO_6"     "6_UP"       "6_UP"      
#' [9] "6_UP"     
labelled_quantile <- function(x){
  pt1 <- quantile(x, probs = seq(0, 1, by = 0.2), type = 7)
  pt2 <- unique(as.data.frame(pt1), fromLast = TRUE)
  
  paste0(pt2[1,],"_TO_",pt2[2,]) -> q1_lab
  paste0(pt2[2,],"_TO_",pt2[3,]) -> q2_lab
  paste0(pt2[3,],"_TO_",pt2[4,]) -> q3_lab
  paste0(pt2[4,],"_TO_",pt2[5,]) -> q4_lab
  paste0(pt2[5,],"_UP") -> q5_lab
  
  pt3 <- rownames(pt2)
  pt4 <- as.integer(strsplit(pt3, "%"))
  datp <- pt4[as.integer(.bincode(x, c(0, pt2$pt1), right=FALSE,include.lowest=TRUE))]
  datp
  
  dplyr::case_when(
    datp == 0 ~ q1_lab,
    datp == 20 ~ q1_lab,
    datp == 40 ~ q2_lab,
    datp == 60 ~ q3_lab,
    datp == 80 ~ q4_lab,
    datp == 100 ~ q5_lab
    )
}

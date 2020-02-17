#' Performs a t-test on NPS
#' 
#' Returns a diagnostic message and the margin of error. 
#' 
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' 
#' @param x NPS vector to be passed through. Use `surveytoolbox::as_nps` to convert a 11-point scale into a NPS vector.
#' @param conf_level Numeric value to specify the confidence level. Defaults to 0.95.
#' @examples
#' x <- sample(0:10, 1000, replace = TRUE)
#' nps_x <- surveytoolbox::as_nps(x)
#' @export

ttest_nps <- function(x, conf_level = 0.95){
  
  test <- t.test(x, conf.level = conf_level)
  
  ci <- test$conf.int
  
  cl <- paste0(round(conf_level * 100, 1), "%")
  
  ci_range <- ci[2] - ci[1]
  
  cir1 <- (test$estimate - ci_range) %>% round(1)
  cir2 <- (test$estimate + ci_range) %>% round(1)
  
  glue::glue("NPS: {test$estimate}. 
  We are {cl} certain that NPS is in the range of {test$estimate} +/- {round((ci_range), 1)};
  the range is {cir1} to {cir2}.
  Alternatively we can say that we are {cl} certain that our reported score is out by no more than {round((ci_range), 1)} points.") %>%
    message()
  message("Returning margin of error:")
  
  return(ci_range)
}

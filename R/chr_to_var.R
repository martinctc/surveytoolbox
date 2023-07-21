#' @title Convert a string / character variable into a labelled double variable,
#' using a pre-specified set of variable and value label mappings.
#' 
#' @description
#' This function takes a string variable and converts it into a labelled double
#' variable, using value label mappings (`lab_str`, `lab_num`) and variable 
#' labels (`var_label`) provided by the user.
#' 
#' @details
#' This function is a wrapper around several other {surveytoolbox} functions:
#'   - `create_named_list()`
#'   - `set_vall()`
#'   - `set_varl()`
#' 
#' @param var string vector containing the variable
#' @param lab_str string vector containing the labels for the variable. The 
#' lengths and values must be an exact match of the variable in `var`.
#' @param lab_num numeric vector matching the labels provided in `lab_str`. The
#' length of `lab_num` must be the same as `lab_str`, and the order is used to 
#' determine the mapping.
#' @param var_label string to be used as the variable label, passed through to
#' `set_varl()`.
#' @param na string to replace missing values with as a value label. Defaults to
#' `"<N/A>"`. Set to `NULL` to not replace missing values. 
#' 
#' @return a labelled double variable
#' 
#' @examples
#' var1 <- c("Yes", "No", "Not sure")
#' lab1 <- c(1, 0, 99)
#' q1_var <- sample(var1, 100, replace = TRUE)
#' 
#' # Convert to labelled double
#' chr_to_var(
#'   var = q1_var,
#'   lab_str = var1,
#'   lab_num = lab1,
#'   var_label = c("Have you come across this product previously?")
#' )
#' 
#' ## Example with missing values
#' # Generate variable with missing values
#' var1 <- c("Yes", "No", NA) 
#' q1_var <- sample(var1, 100, replace = TRUE)
#' 
#' # Variable and value labels
#' var1b <- c("Yes", "No", "<N/A>")
#' lab1 <- c(1, 0, 99)
#' 
#' # Convert to labelled double
#' chr_to_var(
#'   var = q1_var,
#'   lab_str = var1b,
#'   lab_num = lab1,
#'   var_label = c("Have you come across this product previously?")
#' )
#' 
#' @export             

chr_to_var <- function(var, lab_str, lab_num, var_label, na = "<N/A>") {
  
  table <- data.frame(var = lab_str, return = lab_num)
  nm_list <- surveytoolbox::create_named_list(x = lab_str, y = lab_num)
  
  var <- tidyr::replace_na(var, na)
  var <- surveytoolbox::look_up(var, table)
  var <- as.numeric(var)
  var <- surveytoolbox::set_vall(var, value_labels = nm_list)
  var <- surveytoolbox::set_varl(var, var_label)
  
  return(var)
}
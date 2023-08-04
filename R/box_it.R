#' Convert ordinal variables into binary variables by "boxing"
#' 
#' @description
#' For instance, you can create a Top Two Box variable from a 7-point agreement
#' scale question. Function returns a labelled double binary variable, which
#' will have value label attributes.
#' 
#' @param x Variable to be passed through
#' @param which Character string to specify which end of spectrum to take values
#'   - valid inputs are "top" and "bottom".
#' @param number integer value indicating the n to take values from, e.g. 'top
#' n box' or 'bottom n box'
#' @param replace_na numeric vector. These values from the variable are replaced
#'   with `NA_real_`.
#' @param lab_str string vector of length 3 containing the labels for the output
#'   binary variable. The first value maps to 1 (box selected), the second
#'   value maps to 0 (box not selected), and the third values maps to missing
#'   values.
#' @param var_label string to be used as the variable label, passed through to
#' `set_varl()`.
#' @param na_val numeric value or NULL, defaults to 99. When set to a number,
#'   this is the number that is used to represent missing values. When set to
#'   NULL, missing values would be stored as `NA_real_` type.
#' 
#' @return a binary variable of labelled double type.
#' 
#' @examples
#' box_it(sample(1:10,100,replace = TRUE)) # Converted to binary variable where 9, 10 are selected
#' box_it(sample(c(1:10, NA),100,replace = TRUE))
#' 
#' # Example where specified numeric values are replaced with NAs
#' summary(
#'   box_it(
#'     sample(c(1:10, 99), 100, replace = TRUE),
#'     replace_na = 99)
#'   )
#' 
#' @export
box_it <-
  function(x,
           which = "top",
           number = 2,
           replace_na = NULL,
           lab_str = c("Selected", "Not selected", "Missing value"),
           var_label = paste0(which, number),
           na_val = 99) {
  
  # replace numeric values with NA
  if(!is.null(replace_na)){
    x <- ifelse(x %in% replace_na, NA_real_, x)
  }
  
  max_x <- max(x,na.rm = TRUE)
  min_x <- min(x,na.rm = TRUE)
  
  if(which=="top"){
    valid_range <- (max_x - number + 1):max_x
  } else if(which=="bottom"){
    valid_range <- min_x:(min_x + number - 1)
  }
  
  if(is.null(na_val)){
    
    output <-
      dplyr::case_when(is.na(x) ~ NA_real_,
                       x %in% valid_range ~ 1,
                       !(x %in% valid_range) ~ 0,
                       TRUE~NA_real_)
    
    attr(output, 'labels') <- stats::setNames(c(1, 0, NA_real_), lab_str)
    
  } else {
    
    output <-
      dplyr::case_when(is.na(x) ~ NA_real_,
                       x %in% valid_range ~ 1,
                       !(x %in% valid_range) ~ 0,
                       TRUE ~ na_val)
    
    attr(output, 'labels') <- stats::setNames(c(1, 0,  na_val), lab_str)
    
  }
  
  # Set variable labels and make this a labelled double object
  output <- set_varl(output, var_label)
  output <- haven::labelled(output, labels = attr(output, 'labels'))
  
  # Diagnostic messages
  range_print <- paste0(valid_range,collapse=", ")
  message(paste0("Converted to binary variable where ",
                 range_print,
                 " are selected"))
  return(output)
} 
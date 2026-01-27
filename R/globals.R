## Global Variables
##
## This file is added to minimize the false positives flagged during R CMD check.

#' @importFrom graphics hist
#' @importFrom stats chisq.test
#' @importFrom rlang .data sym
NULL

utils::globalVariables(
  c(
    "coef",
    "value",
    "prop",
    ".",
    "write.table",
    "cor_matrix",
    "quantile",
    "read.table",
    "hclust",
    "dist",
    "t.test",
    "var_label",
    "statistic",
    "p",
    "df",
    "method",
    "p.signif",
    "alternative"
  )
)

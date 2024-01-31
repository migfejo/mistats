#' @title Multiple Test T
#' @description Perform the t-test separately for each of the variables in a
#' \code{data.frame}.
#' @param data \code{data.frame} containing the values of the variables for each
#' sample. Values of the variables selected in `vars` must be `numeric`.
#' @param vars String vector containing the names of the variables
#' (matching colnames(data)) to analyze. Values of the selected `vars` must be
#' `numeric`.
#' @param group1 String vector containing names (matching rownames(data)) of
#' the first defined group.
#' @param group2 String vector containing names (matching rownames(data)) of
#' the second defined group.
#' @return Vector containing resulting p-values for each test for each variable.
#' @export multiple.t.test
#'
multiple.t.test <- function(data, vars = colnames(data), group1, group2) {
  res <- c()
  for (var in vars) {
    res <- c(res,
             t.test(data[group1, var], data[group2, var])$p.value)
  }
  names(res) <- vars
  res
}

#' @title Multiple Analysis of Variance
#' @description Perform ANOVA separately for each of the variables in a
#' \code{data.frame}.
#' @param data \code{data.frame} containing the values of the variables for each
#' sample. Values of the variables selected in \code{vars} must be \code{numeric}.
#' @param group.var Vector containing group identifiers. Its length must match
#' with the number of \code{data} rows, and its elements must be in the same order.
#' @param vars Default = \code{colnames(data)}. String vector containing the names of the variables
#' (matching \code{colnames(data)}) to analyze. Values of the selected \code{vars} must be
#' \code{numeric}.
#' @return Vector containing resulting p-values for each test for each variable.
#' @export multiple.anova
#'
multiple.anova <- function(data, group.var, vars = colnames(data)){
  df <- cbind(Group = factor(group.var), data)
  res <- c()
  for (var in vars) {
    res <- c(res,
             summary(stats::aov(stats::as.formula(paste0("`", var, "` ~ Group")), data = df))[[1]][1, "Pr(>F)"]
    )
  }
  names(res) <- vars
  res
}

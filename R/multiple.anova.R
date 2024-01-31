#' @title Multiple Analysis of Variance
#' @description Perform ANOVA separately for each of the variables in a
#' `data.frame`.
#' @param data data.frame containing the values of the variables for each
#' sample. Values of the variables selected in `vars` must be `numeric`.
#' @param group.var Vector containing group identifiers. Its length must match
#' with the number of `data` rows, and its elements must be in the same order.
#' @param vars String vector containing the names of the variables
#' (matching colnames(data)) to analyze. Values of the selected `vars` must be
#' `numeric`.
#' @return Vector containing resulting p-values for each test for each variable.
#' @export multiple.t.test
#'
multiple.anova <- function(data, group.var, vars = colnames(data)){
  df <- cbind(Group = factor(group.var), data)
  res <- c()
  for (var in vars) {
    res <- c(res,
             summary(aov(as.formula(paste0("`", var, "` ~ Group")), data = df))[[1]][1, "Pr(>F)"]
    )
  }
  names(res) <- vars
  res
}

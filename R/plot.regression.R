#' Plot Linear Regression Model
#' @description Plots a scatter plot of 2D data, and overlaps its regression
#' model line. The plot is based on R base plotting packages.
#' @inheritParams stats::lm
#' @inherit stats::lm return
#' @export plot.regression
#'
plot.regression <- function(formula, data, ...) {
  base::plot(formula = stats::as.formula(formula), data = data)
  graphics::abline(stats::lm(formula = stats::as.formula(formula), data = data, ...),
                   col = "red")
  stats::lm(formula = stats::as.formula(formula), data = data, ...)
}

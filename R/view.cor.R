#' @title Filter and View Correlation Assessment
#' @description This function outputs the correlation results of multiple
#' variables as a `numeric` vector, and can be filtered by a minimum threshold.
#' @param vars1 `data.frame` with the first set of variables, in columns, to
#' compare with the second set from `vars2`. Observations are in rows, and they
#' must match in order with those in `vars2`.
#' @param vars2 `data.frame` with the second set of variables, in columns, to
#' compare with the first set from `vars1`. Observations are in rows, and they
#' must match in order with those in `vars1`.
#' @return `numeric` vector with the results of the assessed correlations.
#' @export view.cor
#' @examples
#'
view.cor <- function(vars1, vars2, threshold = 0.5, method = "pearson"){
  corrs <- cor(vars1, vars2, method = method)
  indexes <- which(abs(corrs) >= threshold)
  res <- c()
  resnames <- c()
  for (i in indexes){
    if (i %% nrow(corrs) == 0) {var1.name <- rownames(corrs)[nrow(corrs)]} else {
      var1.name <- rownames(corrs)[i %% nrow(corrs)]
    }
    if (i %% nrow(corrs) == 0) {var2.name <- colnames(corrs)[i %/% nrow(corrs)]} else {
      var2.name <- colnames(corrs)[i %/% nrow(corrs) + 1]
    }
    res <- c(res, corrs[var1.name, var2.name])
    resnames <- c(resnames, paste0(var1.name, " - ", var2.name))
  }
  names(res) <- resnames
  res
}

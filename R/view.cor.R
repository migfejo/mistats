#' @title Filter and View Correlation Assessment
#' @description This function outputs the correlation results of multiple
#' variables as a \code{numeric} vector, and can be filtered by a minimum threshold.
#' @param vars1 \code{data.frame} with the first set of variables, in columns, to
#' compare with the second set from \code{vars2}. Observations are in rows, and they
#' must match in order with those in \code{vars2}.
#' @param vars2 \code{data.frame} with the second set of variables, in columns, to
#' compare with the first set from \code{vars1}. Observations are in rows, and they
#' must match in order with those in \code{vars1}.
#' @param threshold Minimum threshold value for filtering correlation results.
#' It applies to positive and negative correlations.
#' @param method Correlation measure to assess. Options are the same as for
#' \code{\link[stats]{cor}}. Default = \code{"pearson"}.
#' @return \code{numeric} vector with the results of the assessed correlations.
#' @export view.cor
#'
view.cor <- function(vars1, vars2, threshold = 0.5, method = "pearson"){
  corrs <- stats::cor(vars1, vars2, method = method)
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

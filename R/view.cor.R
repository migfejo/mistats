#' Filter and View Correlation Assessment
#'
#' @description This function outputs the correlation results of multiple
#' variables as a \code{numeric} vector, and can be filtered by a minimum threshold.
#'
#' @param x,y \code{data.frames} with the first and the second sets of variables,
#' respectively, to be compared each other. Variables must be in columns.
#' Observations must be in rows, and in the same order in both sets.
#'
#' If only \code{x} is given as a matrix or dataframe, correlations of its
#' columns will be calculated.
#'
#' @param threshold Minimum threshold value for filtering correlation results.
#' It applies to positive and negative correlations.
#'
#' @param method Correlation measure to assess. Options are the same as for
#' \code{\link[stats]{cor}}. Default = \code{"pearson"}.
#'
#' @return \code{numeric} vector with the results of the assessed correlations.
#'
#' @export view.cor
#'
view.cor <- function(x, y = NULL, threshold = 0.5, method = "pearson"){
  corrs <- stats::cor(x, y, method = method)
  if (is.null(y) | identical(x, y)) {  # If corrs is square matrix, make upper triangle zero
    for (r in 1:nrow(corrs)) {
      for (c in 1:ncol(corrs)) {
        if (c >= r) {corrs[r, c] <- 0}
      }
    }
  }
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


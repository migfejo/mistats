#' Heatmap Highlight
#'
#' @description
#' Takes a \code{data.frame} and plots the matches with a given boolean
#' expression in a NMF based heatmap.
#'
#' @param data
#' \code{data.frame} in which boolean expressions are going to be tested.
#'
#' @param bool
#' Simple boolean expression, given as a character string, first the operator,
#' then the number.
#'
#' @param mode
#' Only needed when no boolean expression is given in \code{bool}.
#' \code{"is.na"} \code{mode} is for searching for NA values in \code{data}, and
#' \code{"isnot.na"} \code{mode} is for searching for not NA in \code{data}.
#'
#' @inheritDotParams NMF::aheatmap
#'
#' @importFrom NMF aheatmap
#'
#' @return Generates a plot based on \code{\link[NMF]{aheatmap}} function.
#' @export heatmap.highlight
#'
heatmap.highlight <- function(data, bool = NULL, mode = "isnot.na", ...) {
  if (is.null(bool)) {  # If no boolean expression if given
    if (mode == "is.na") {  # If searching NA mode is set
      indexes <- which(eval(parse(text = paste0("is.na(", substitute(data), ")"))))
    }
    if (mode == "isnot.na") {  # If searching not NA mode is set
      indexes <- which(eval(parse(text = paste0("!is.na(", substitute(data), ")"))))
    }
  }
  else {
    indexes <-  which(eval(parse(text = paste0(as.character(substitute(data)), bool))))
  }

  if (length(indexes) != 0) {  # Check if there is results
    mat <- matrix(nrow = nrow(data), ncol = ncol(data))
    rownames(mat) <- rownames(data)
    colnames(mat) <- colnames(data)
    mat[indexes] <- as.matrix(data)[indexes]

    mtext <- mat

    if (is.character(mat)) {
      nmat <- matrix(nrow = nrow(mat), ncol = ncol(mat))
      rownames(nmat) <- rownames(mat)
      colnames(nmat) <- colnames(mat)
      nmat[indexes]    <- 1
      nmat[is.na(mat)] <- 0
    }

    if(is.numeric(mat)) {
      nmat  <- mat
      mtext <- signif(mat, digits = 5)
    }

    NMF::aheatmap(nmat, Rowv = NA, Colv = NA, txt = mtext, ...)
  }

  else {cat("No results found. No plot generated.\n")}
}

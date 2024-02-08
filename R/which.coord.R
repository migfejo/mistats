#' Logical Dataframe Coordinates Finder
#'
#' @description
#' Find positions or coordinates in a \code{data.frame} which match with a
#' logical expression.
#'
#'
#' @param data \code{data.frame} in whose coordinates are being looking for.
#' @param bool Simple boolean expression, given as a character string, first the
#' operator, then the number.
#' @param mode Only needed when no boolean expression is given in \code{bool}.
#' \code{"is.na"} \code{mode} is for searching for NA values in \code{data}, and
#' \code{"isnot.na"} \code{mode} is for searching for not NA in \code{data}.
#'
#' @return \code{data.frame} where every row is a match in the logical search.
#' The names of the row and column from \code{data} is given, as well as the
#' value it contains.
#'
#' @export which.coord
#'
which.coord <- function(data, bool = NULL, mode = "isnot.na") {
  if (is.null(bool)) {  # If no boolean expression if given
    if (mode == "is.na") {  # If searching NA mode is set
      indexes <- which(eval(parse(text = paste0("is.na(", substitute(data), ")"))),
                       arr.ind = TRUE)
    }
    if (mode == "isnot.na") {  # If searching not NA mode is set
      indexes <- which(eval(parse(text = paste0("!is.na(", substitute(data), ")"))),
                       arr.ind = TRUE)
    }
  }
  else {
    indexes <-  which(eval(parse(text = paste0(as.character(substitute(data)), bool))),
                      arr.ind = TRUE)
  }

  if (length(indexes) != 0) {  # Check if there is results
    res <- data.frame(rowname = rownames(data)[indexes[, "row"]],
                      colname = colnames(data)[indexes[, "col"]])

    for (i in 1:nrow(indexes)) {
      res[i, "value"] <- data[indexes[i, "row"], indexes[i, "col"]]
    }

    res

  }
  else {
    NULL
  }
}

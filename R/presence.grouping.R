#' Presence/Absence Grouping
#'
#' @description
#' Takes a \code{data.frame} and classifies each \code{numeric}
#' variable into two sample groups: presence (unequal to zero) and absence
#' (equal to zero).
#'
#' @param data
#' \code{data.frame} with samples in rows and variables in columns.
#'
#' @param vars
#' Variables from \code{data} to be classified. All of them must be \code{numeric}.
#'
#' @return
#' A nested list. Each element (variable) contains a character vector
#' of the names of the samples from each group, presence and absence, respectively,
#' and \code{NA} if applicable.
#'
#' @export presence.grouping
#'
presence.grouping <- function(data, vars = colnames(data)) {
  po <- list()  # po --> presence object
  po <- po[vars]
  names(po) <- vars

  for (x in vars) {
    po[[x]]$presence <- rownames(data[data[, x] != 0 &
                                        !is.na(data[, x]), ])
    po[[x]]$absence  <- rownames(data[data[, x] == 0 &
                                        !is.na(data[, x]), ])
    if (anyNA(data[, x])) {
      po[[x]]$na <- rownames(data[is.na(data[, x]), ])
    }
  }

  po
}




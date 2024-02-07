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
#' @param lower.limit
#' Minimum number allowed of members in each group. Minimum for t test is 2.
#'
#' @return
#' A nested list. Each element (variable) contains a character vector
#' of the names of the samples from each group, presence and absence, respectively,
#' and \code{NA} if applicable.
#'
#' @export presence.grouping
#'
presence.grouping <- function(data, vars = colnames(data), lower.limit = 2) {
  po <- list()  # po --> presence object
  for (x in vars) {
    if (all(nrow(data[data[, x] != 0 & !is.na(data[, x]), ]) >= lower.limit,
            nrow(data[data[, x] == 0 & !is.na(data[, x]), ]) >= lower.limit)) {
      po[[x]]$presence <- rownames(data[data[, x] != 0 &
                                          !is.na(data[, x]), ])
      po[[x]]$absence  <- rownames(data[data[, x] == 0 &
                                          !is.na(data[, x]), ])
      if (anyNA(data[, x])) {
        po[[x]]$na <- rownames(data[is.na(data[, x]), ])
      }
    }
  }

  po
}





#' Dichotomic Boxplot with t.test
#'
#' @param data
#' \code{data.frame} or \code{matrix} containing data.
#' @param y
#' \code{character} string. The name of the \code{numeric} variable to plot whose
#' distribution will be shown through boxplots.
#' @param x
#' \code{character} string. The name of the categoric variable determining groups.
#' In case of dichotomic variable, a t test will be performed.
#' @param facet
#' \code{character} string, optional.
#' Variable chosen for divide boxplots through different categories.
#'
#' @return A \code{\link{ggplot2}} object representing boxplots of different groups.
#' A t test p.value is shown when group number is 2.
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter xlab ylab theme element_text ggtitle facet_wrap
#'
#' @export plot.ggboxplot
#'
plot.ggboxplot <- function(data, y, x, facet = NULL) {
  p.value <- "NOT AVAILABLE"
  p <- ggplot2::ggplot(data, ggplot2::aes(x = eval(parse(text = x)),
                                          y = eval(parse(text = y)),
                                          colour = eval(parse(text = x)))) +
    ggplot2::geom_boxplot(outlier.alpha = 0) +
    ggplot2::geom_jitter(width = 0.2, height = 0) +
    ggplot2::xlab(x) + ggplot2::ylab(y) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 1),
                   legend.position = "none")

  if (is.null(facet)) {  # No facets segregation
    if (length(unique(data[, x])) == 2) {  # t.test performing if x is dichotomic variable
      p.value <- stats::t.test(as.formula(paste0(y, " ~ ", x)), data = data)$p.value
      p.value <- signif(p.value, 5)
    }
    p + ggplot2::ggtitle(paste0("p.value = ", p.value))
  }
  else {  # Divide plot into facets
    p + ggplot2::ggtitle(paste0("p.value = ", p.value)) +
      ggplot2::facet_wrap(~ eval(parse(text = facet)), ncol = 2, scales = "free")
  }
}

#' PERMANOVA analysis
#'
#' @description
#' It performs a PERMANOVA analysis through \code{\link[vegan]{adonis2}} multiple times and
#' it takes the mean of the all resulting p-value as a more robust method than
#' doing just 1 iteration of PERMANOVA.
#'
#' @param physeq
#' A phyloseq object from package \pkg{phyloseq}.
#'
#' @param method
#' Short string indicating the distance measure to take into account for beta
#' diversity calculation. It has to be the same as the accepted strings for the
#' function \code{\link[phyloseq]{distance}}. For example:
#' \itemize{
#'   \item \code{bray}: Bray-Curtis
#'   \item \code{jaccard}: Jaccard.
#'   \item \code{unifrac}: Original (unweighted) UniFrac distance.
#'   \item \code{wunifrac}: Weighted-UniFrac distance.
#'   \item ...
#' }
#'
#' @param variables
#' Vector of strings containing the name, always in first position, of the
#' metadata variable to analyse. Following variables will be included in the
#' analysis as confounders.
#'
#' @param permutations
#' Number of permutations for PERMANOVA analysis (\code{\link[vegan]{adonis2}}).
#'
#' @param repetitions
#' Number of times that PERMANOVA analysis will be performed.
#'
#' @param print.results
#' If \code{TRUE}, results of the PERMANOVA analysis will be printed in console.
#'
#' @param show.plot
#' If \code{TRUE}, PCoA plot of the beta diversity analysis will be ploted in Rstudio.
#'
#' @param filename
#' If \code{NULL}, no plot will be saved. Otherwise, provide the string name of the file
#' that will contain the PCoA plot of the beta diversity analysis.
#'
#'
#' @import ggplot2
#' @import phyloseq
#' @importFrom vegan adonis2
#'
#'
#' @export permanova.beta
#'
permanova.beta <- function(physeq, method = "bray", variables, permutations = 999, repetitions = 1,
                           print.results = TRUE, show.plot = TRUE, filename = NULL) {
    ## PERMANOVA analysis
    p.values <-
      sapply(c(1:repetitions), function(x) {
        adonis2(
          formula = formula(paste0("phyloseq::distance(physeq, method = method) ~ ",
                                   paste(variables, collapse = " + "))),
          data = data.frame(phyloseq::sample_data(physeq),
                            chek.names = FALSE),
          permutations = 999,
          by = "margin")[variables[1], "Pr(>F)"]
        }
      )


    ## Print results
    if (print.results) {
      cat("\n")
      cat(rep("-", 21), sep = "")
      cat("\n")
      cat("| PERMANOVA RESULTS |")
      cat("\n")
      cat(rep("-", 21), sep = "")
      cat("\n")
      cat("\n")
      cat("|       Method = ", method)
      cat("\n")
      cat("| Permutations = ", permutations)
      cat("\n")
      cat("|  Repetitions = ", method)
      cat("\n")
      cat("\n")
      cat("Obtained p-values:")
      cat("\n")
      cat(p.values)
      cat("\n")
      cat("\n")
      cat(paste0("|               Mean p-values = ", mean(p.values)))
      cat("\n")
      cat(paste0("| Standard deviation p-values = ", sd(p.values)))
      cat("\n")
      cat("\n")
      cat(rep("-", 17), sep = "")
      cat("\n")
      cat("| END OF REPORT |")
      cat("\n")
      cat(rep("-", 17), sep = "")
      cat("\n")
      cat("\n")
    }


    ## Hacer el plot
    if (show.plot | is.character(filename)) {  # Plot only is made if showing or saving
      ## PCoA and extracting coordinate percentages
      pcoa <- phyloseq::ordinate(physeq, method = "PCoA", distance = method)
      co1 <- round(pcoa$values$Eigenvalues[1] / sum(pcoa$values$Eigenvalues) * 100, 2)
      co2 <- round(pcoa$values$Eigenvalues[2] / sum(pcoa$values$Eigenvalues) * 100, 2)

      ## Making p-value string to add in graph
      p.line <-
        ifelse(test = mean(p.values) < 0.001,
               yes = "p-value < 0.001",
               no = paste0("p-value = ", round(mean(p.values), 3)))

      ## Plotting
      beta.plot <-
        phyloseq::plot_ordination(physeq,
                                  pcoa,
                                  color = variables[1]) +
        theme_bw() +
        stat_ellipse(level = 0.95, type = "norm", linetype = 2) +
        # scale_color_manual(values = c("deepskyblue3", "firebrick3")) +
        labs(color = variables[1],
             title = paste0("PCoA beta diversity ", method),
             x = paste0("Coordinate 1 (", co1, "%)"),
             y = paste0("Coordinate 2 (", co2, "%)")) +
        annotate("text",
                 label = p.line,
                 x = -Inf, y = Inf,        # Esquina superior izquierda
                 hjust = -0.1, vjust = 1.5,
                 fontface = "bold") +
        theme(plot.title = element_text(hjust = 0.5),
              title = element_text(face = "bold"))
    }


    ## Printing plot in Rstudio if desired
    if (show.plot) print(beta.plot)


    ## Saving plot if desired
    if (is.character(filename)) {
      ggsave(filename = filename,
             plot = beta.plot,
             dpi = 300,
             height = 6,
             width = 8)
    }
}

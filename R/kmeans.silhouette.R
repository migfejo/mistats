#' Silhouette Method for K-means Clustering
#'
#' @description This function performs Silhouette method in order to assess the
#' optimal number of clusters for given data, when clustering by K-means
#' algorithm.
#'
#' @param data \code{data.frame} with samples in rows and variables in columns.
#'
#' @param print.result Default = \code{FALSE}. If \code{TRUE}, the optimal number of clusters
#' and its Silhouette score is printed as a message in the R terminal.
#'
#' @param print.plot Default = \code{FALSE}. If \code{TRUE}, a simple scatter plot will be
#' generated showing the silhouette scores for each potential number of
#' clusters.
#'
#' @return \code{numeric} value. Number of optimal clusters.
#'
#' @importFrom cluster silhouette
#'
#' @export kmeans.silhouette
#'
kmeans.silhouette <- function(data, print.result = FALSE, print.plot = FALSE){
  scores <- c()
  for (i in 2:(nrow(data) - 1)){
    scores[as.character(i)] <- mean(cluster::silhouette(stats::kmeans(data, i)$cluster,
                                                        stats::dist(data))[, 3])
  }
  if (print.result){
    cat("Optimal amount of cluster is ",
        names(scores[which(scores == max(scores))]),
        " with a silhouette score of ",
        max(scores),
        "\n",
        sep = "")
  }
  if (print.plot){plot(scores)}
  as.numeric(names(scores[which(scores == max(scores))]))
}

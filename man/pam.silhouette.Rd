% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pam.silhouette.R
\name{pam.silhouette}
\alias{pam.silhouette}
\title{Silhouette Method for Partitioning Around Medoids Clustering}
\usage{
pam.silhouette(
  data,
  distance = "bray",
  print.result = FALSE,
  print.plot = FALSE
)
}
\arguments{
\item{data}{\code{data.frame} with samples in rows and variables in columns.}

\item{distance}{Default is \code{bray}, i.e., Bray-Curtis dissimilarity. It can
be used any measure of distance or dissimilarity supported by the function
\code{\link[vegan]{vegdist}}.}

\item{print.result}{Default = \code{FALSE} If \code{TRUE}, the optimal number of clusters
and its Silhouette score is printed as a message in the R terminal.}

\item{print.plot}{Default = \code{FALSE} If \code{TRUE}, a simple scatter plot will be
generated showing the silhouette scores for each potential number of
clusters.}
}
\value{
Number of optimal clusters.
}
\description{
This function performs Silhouette method in order to assess the
optimal number of clusters for given data, when clustering by PAM algorithm.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiple.t.test.R
\name{multiple.t.test}
\alias{multiple.t.test}
\title{Multiple Test T}
\usage{
multiple.t.test(data, group1, group2, vars = colnames(data))
}
\arguments{
\item{data}{\code{data.frame} containing the values of the variables for each
sample. Values of the variables selected in \code{vars} must be \code{numeric}.}

\item{group1}{String vector containing names (matching \code{rownames(data)}) of
the first defined group.}

\item{group2}{String vector containing names (matching \code{rownames(data)}) of
the second defined group.}

\item{vars}{String vector containing the names of the variables
(matching \code{colnames(data)}) to analyze. Values of the selected \code{vars} must be
\code{numeric}.}
}
\value{
Vector containing resulting p-values for each test for each variable.
}
\description{
Perform the t-test separately for each of the variables in a
\code{data.frame}.
}

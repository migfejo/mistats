% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/which.coord.R
\name{which.coord}
\alias{which.coord}
\title{Logical Dataframe Coordinates Finder}
\usage{
which.coord(data, bool = NULL, mode = "isnot.na")
}
\arguments{
\item{data}{\code{data.frame} in whose coordinates are being looking for.}

\item{bool}{Simple boolean expression, given as a character string, first the
operator, then the number.}

\item{mode}{Only needed when no boolean expression is given in \code{bool}.
\code{"is.na"} \code{mode} is for searching for NA values in \code{data}, and
\code{"isnot.na"} \code{mode} is for searching for not NA in \code{data}.}
}
\value{
\code{data.frame} where every row is a match in the logical search.
The names of the row and column from \code{data} is given, as well as the
value it contains.
}
\description{
Find positions or coordinates in a \code{data.frame} which match with a
logical expression.
}

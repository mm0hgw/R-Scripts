% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mung.R
\name{mung}
\alias{mung}
\title{mung}
\usage{
mung(x, mungfuns = c("max", "min", "mean"), key = gsub("[\\\\.0-9]", "",
  colnames(x)))
}
\arguments{
\item{x}{a data.frame to mung.}

\item{mungfuns}{a vector of function names to mung with.}

\item{key}{a 'vector' mapping which columns mung together}
}
\value{
a 'matrix' containing the munged results.
}
\description{
MUNG UNTIL NO GOOD
}

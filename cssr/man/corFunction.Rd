% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{corFunction}
\alias{corFunction}
\title{Absolute value of sample correlation between two vectors}
\usage{
corFunction(t, y)
}
\arguments{
\item{t}{A numeric or integer vector.}

\item{y}{A numeric or integer vector; must have the same length as t.}
}
\value{
A numeric vector of the same length as cluster_i containing the
weights corresponding to each of the features in cluster_i. The weights
will all be nonnegative and sum to 1.
}
\description{
Calculates the absolute value of correlation of t and y. If either input has
only one unique value, returns 0 by definition.
}
\author{
Gregory Faletto, Jacob Bien
}
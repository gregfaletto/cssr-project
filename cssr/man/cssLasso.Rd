% Generated by roxygen2: do not edit by hand
% Please edit documentation in create-cssr.Rmd.
\name{cssLasso}
\alias{cssLasso}
\title{Provided fitfun implementing the lasso}
\usage{
cssLasso(X, y, lambda)
}
\arguments{
\item{X}{A design matrix containing the predictors. (In practice this will
be a subsample of the full design matrix provided to css.)}

\item{y}{A numeric vector containing the response.}

\item{lambda}{Numeric; a nonnegative number for the lasso penalty to use
on each subsample. (For now, only one lambda value can be provided to
cssLasso; in the future, we plan to allow for multiple lambda values to be
provided to cssLasso, as described in Faletto and Bien 2022.)}
}
\value{
An integer vector; the indices of the features selected by the lasso.
}
\description{
Function used to select features with the lasso on each subsample in cluster
stability selection. Uses glmnet implementation of the lasso.
}
\references{
Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
\emph{arXiv preprint arXiv:2201.00494}.
\url{https://arxiv.org/abs/2201.00494}. \cr Jerome Friedman, Trevor Hastie,
Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models
via Coordinate Descent. \emph{Journal of Statistical Software}, 33(1), 1-22.
URL \url{https://www.jstatsoft.org/v33/i01/}.
}
\author{
Gregory Faletto, Jacob Bien
}
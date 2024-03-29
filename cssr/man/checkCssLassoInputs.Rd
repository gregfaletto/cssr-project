% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{checkCssLassoInputs}
\alias{checkCssLassoInputs}
\title{Helper function to confirm that the inputs to \code{cssLasso()} are as expected.}
\usage{
checkCssLassoInputs(X, y, lambda)
}
\arguments{
\item{X}{A design matrix containing the predictors. (Note that we don't need
to check X very much, because X will have already been checked by the
function \code{checkCssInputs()} when it was provided to \code{css()}.)}

\item{y}{A numeric vector containing the response.}

\item{lambda}{Numeric; a nonnegative number for the lasso penalty to use
on each subsample. (For now, only one lambda value can be provided to
\code{cssLasso()}; in the future, we plan to allow for multiple lambda values to be
provided to \code{cssLasso()}, as described in Faletto and Bien 2022.)}
}
\description{
Helper function to confirm that the inputs to \code{cssLasso()} are as expected.
}
\author{
Gregory Faletto, Jacob Bien
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in create-cssr.Rmd.
\name{cssLoop}
\alias{cssLoop}
\title{Helper function run on each subsample}
\usage{
cssLoop(input, x, y, lambda, fitfun)
}
\arguments{
\item{x}{an n x p numeric matrix containing the predictors. (This should be
the full design matrix provided to css.)}

\item{y}{A response; can be any response that takes the form of a length n
vector and is used (or not used) by fitfun. Typically (and for default fitfun
= cssLasso), y should be an n-dimensional numeric vector containing the
response. This should be the full response provided to css.}

\item{lambda}{A tuning parameter or set of tuning parameters that may be used
by the feature selection method. For example, in the default case when
fitfun = cssLasso, lambda is a numeric: the penalty to use for each lasso
fit.}

\item{fitfun}{A function that takes in arguments X, y, and lambda and returns
a vector of indices of the columns of X (selected features).}
}
\value{
An integer vector; the indices of the features selected by fitfun.
}
\description{
Runs provided feature selection method fitfun on each subsample for cluster
stability selection (this function is called within mclapply).
}
\author{
Gregory Faletto, Jacob Bien
}
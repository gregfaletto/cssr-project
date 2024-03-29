% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{checkGetXglmnetInputs}
\alias{checkGetXglmnetInputs}
\title{Verifies the inputs for getXglmnet.}
\usage{
checkGetXglmnetInputs(x, clusters, type, prototypes)
}
\arguments{
\item{x}{A numeric matrix; the provided matrix with n observations and p
features.}

\item{clusters}{A named list where each entry is an integer vector of indices
of features that are in a common cluster. (The length of list clusters should
be equal to the number of clusters.) All identified clusters should be
non-overlapping. All features should appear in exactly one cluster (any
unclustered features should be put in their own "cluster" of size 1).}

\item{type}{Character; "protolasso" for the protolasso or "clusterRepLasso"
for the cluster representative lasso.}

\item{prototypes}{Only required for type "protolasso". An integer vector
whose length is equal to the number of clusters. Entry i should be the
prototype for cluster i (the feature belonging to cluster i that is most
highly correlated with y; see Reid and Tibshirani 2016).}
}
\description{
Verifies the inputs for getXglmnet.
}
\references{
Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
\url{https://doi.org/10.1093/biostatistics/kxv049}.
}
\author{
Gregory Faletto, Jacob Bien
}
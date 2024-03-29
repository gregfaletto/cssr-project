% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{getSelectedSets}
\alias{getSelectedSets}
\title{Converts a selected set from X_glmnet to selected sets and selected clusters
from the original feature space of X.}
\usage{
getSelectedSets(lasso_set, clusters, prototypes, feat_names)
}
\arguments{
\item{lasso_set}{A vector containing the indices of selected cluster
representatives or prototypes.}

\item{clusters}{A named list where each entry is an integer vector of indices
of features that are in a common cluster. (The length of list clusters is
equal to the number of clusters.) All identified clusters must be
non-overlapping. All features appear in exactly one cluster (any unclustered
features must be in their own "cluster" of size 1).}

\item{prototypes}{An integer vector whose length must be equal to the number
of clusters. Entry i should be the index of the feature belonging to cluster
i that is most highly correlated with y (that is, the prototype for the
cluster, as in the protolasso).}

\item{feat_names}{Character vector; the names of the features in X.}
}
\value{
A list containing two items: \item{selected_set}{An integer vector
with length equal to lasso_set containing a set of selected features in the
original X matrix. (Selections in lasso_set corresponding to a cluster will
be replaced by the cluster's prototype from X.)}
\item{selected_clusts_list}{A named list of integer vectors with length equal
to selected_set. selected_clusts_list[\link{k}] will be an integer vector
containing the indices of the features in X that are in the cluster
containing prototype selected_set\link{k}.}
}
\description{
Converts a selected set from X_glmnet to selected sets and selected clusters
from the original feature space of X.
}
\author{
Gregory Faletto, Jacob Bien
}
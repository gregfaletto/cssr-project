% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{checkGetSelectedClustersOutput}
\alias{checkGetSelectedClustersOutput}
\title{Helper function to check that output of getSelectedClusters is as expected}
\usage{
checkGetSelectedClustersOutput(
  selected_clusts,
  selected_feats,
  weights,
  n_clusters,
  p
)
}
\arguments{
\item{selected_clusts}{A named numeric vector containing the selection
proportions for the selected clusters. The name of each entry is the name of
the corresponding cluster.}

\item{selected_feats}{A named integer vector; the indices of the features
with nonzero weights from all of the selected clusters.}

\item{weights}{A named list of the same length as the number of selected
clusters. Each list element weights[\link{j}] is a numeric vector of the weights
to use for the jth selected cluster, and it has the same name as the cluster
it corresponds to.}

\item{n_clusters}{Integer; the number of clusters in the data (upper bound
for the length of selected_clusts)}

\item{p}{Integer; number of features in the data (all selected_feats should
be in 1:p)}
}
\description{
Helper function to check that output of getSelectedClusters is as expected
}
\author{
Gregory Faletto, Jacob Bien
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{print.cssr}
\alias{print.cssr}
\title{Print cluster stability selection output}
\usage{
\method{print}{cssr}(x, cutoff = 0, min_num_clusts = 1, max_num_clusts = NA, ...)
}
\arguments{
\item{x}{An object of class "cssr" (the output of the function css).}

\item{cutoff}{Numeric; print.cssr will display only those
clusters with selection proportions equal to at least cutoff. Must be between
0 and 1. Default is 0 (in which case either all clusters are displayed, or
max_num_clusts are, if max_num_clusts is specified).}

\item{min_num_clusts}{Integer or numeric; the minimum number of clusters to
use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
min_num_clusts clusters, the cutoff will be increased until at least
min_num_clusts clusters are selected.) Default is 1.}

\item{max_num_clusts}{Integer or numeric; the maximum number of clusters to
use regardless of cutoff. (That is, if the chosen cutoff returns more than
max_num_clusts clusters, the cutoff will be decreased until at most
max_num_clusts clusters are selected.) Default is NA (in which case
max_num_clusts is ignored).}

\item{...}{Additional arguments to generic print.data.frame function}
}
\value{
A data.frame; each row contains a cluster, arranged in decreasing
order of cluster selection proportion from top to bottom. The columns are
ClustName (the name of the cluster that was either provided to css or made by
css if no name was provided); ClustProtoName (the name of the selection
prototype from the cluster, which is the feature with the greatest individual
selection proportion among all the cluster members, with ties broken by
choosing the feature with the highest correlation with the response if the
response is real-valued; only returned if the features are named),
ClustProtoNum (the column number of the prototype in the X matrix provided to
css), and ClustSize (the size of the cluster).
}
\description{
Print a summary of the information from the css function (using output from
printCssDf function).
}
\author{
Gregory Faletto, Jacob Bien
}
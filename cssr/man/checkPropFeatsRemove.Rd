% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{checkPropFeatsRemove}
\alias{checkPropFeatsRemove}
\title{Helper function to confirm that the argument prop_feats_remove to several
functions is as expected}
\usage{
checkPropFeatsRemove(prop_feats_remove, p)
}
\arguments{
\item{prop_feats_remove}{Numeric; proportion of features that are dropped on
each subsample. Must be between 0 and 1.}

\item{p}{The number of features; must be greater than 2 if prop_feats_remove
is greater than 0.}
}
\description{
Helper function to confirm that the argument prop_feats_remove to several
functions is as expected
}
\author{
Gregory Faletto, Jacob Bien
}
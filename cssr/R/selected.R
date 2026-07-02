# Generated from _main.Rmd: do not edit by hand

#' Extract the selected clusters or features from cluster stability selection
#'
#' A convenient S3 accessor for the selected clusters (the default) or the
#' selected features of a fitted `cssr` object, without re-running the
#' (computationally expensive) subsampling. This is a thin wrapper around
#' [getCssSelections()].
#'
#' Note that, unlike the `selected()` accessor in the \pkg{stabs} package (which
#' returns the selected *variables*), the default here returns the selected
#' *clusters* -- the natural unit of cluster stability selection. Pass
#' `type = "features"` to obtain the flat integer vector of selected features
#' instead.
#'
#' @param object An object of class "cssr" (the output of the function [css()]).
#' @param ... Additional arguments passed to methods (currently unused).
#' @return For the `cssr` method: if `type = "clusters"` (the default), a named
#' list of integer vectors (each the indices of the features in one selected
#' cluster; an empty list if no cluster is selected); if `type = "features"`, a
#' named integer vector of the indices of the selected features (an empty integer
#' vector if none).
#' @author Gregory Faletto, Jacob Bien
#' @references
#' Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
#' @seealso [summary.cssr()] for an overview (counts plus a per-cluster table);
#' [getCssSelections()] for the underlying selection (clusters, features, and
#' weights together); [printCssDf()] and [print.cssr()] for the printed summary.
#' @examples
#' set.seed(1)
#' data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
#'   cluster_size = 4, n_clusters = 1, snr = 3)
#' clusters <- list(cluster1 = 1:4)
#' res <- css(X = data$X, y = data$y, lambda = 0.01, clusters = clusters,
#'   B = 10)
#' # Selected clusters (the default):
#' selected(res)
#' # Selected features (a flat integer vector):
#' selected(res, type = "features")
#' @export
selected <- function(object, ...){
    UseMethod("selected")
}

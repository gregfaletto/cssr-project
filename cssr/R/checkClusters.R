# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that the argument clusters to several functions is
#' as expected
#'
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of css or
#' formatClusters. (The length of list clusters is equal to the number of
#' clusters.) All identified clusters must be non-overlapping, and all features
#' must appear in exactly one cluster (any unclustered features should be in
#' their own "cluster" of size 1).
#' @param p The number of features; must be at least as large as the number of
#' clusters.
#' @author Gregory Faletto, Jacob Bien
checkClusters <- function(clusters, p){
    stopifnot(is.list(clusters))
    stopifnot(all(lengths(clusters) >= 1))
    stopifnot(all(!is.na(clusters)))

    n_clusters <- length(clusters)

    stopifnot(n_clusters == length(unique(clusters)))
    stopifnot(n_clusters <= p)
    stopifnot(!is.null(names(clusters)))
    stopifnot(is.character(names(clusters)))
    stopifnot(all(!is.na(names(clusters)) & names(clusters) != ""))
    stopifnot(length(unique(names(clusters))) == n_clusters)

    all_clustered_feats <- integer()
    for(i in 1:n_clusters){
        stopifnot(is.integer(clusters[[i]]))
        all_clustered_feats <- c(all_clustered_feats, clusters[[i]])
    }

    stopifnot(length(all_clustered_feats) == p)
    stopifnot(length(unique(all_clustered_feats)) == p)
    stopifnot(all(all_clustered_feats <= p))
    stopifnot(all(all_clustered_feats >= 1))
}

# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to check that output of getSelectedClusters is as expected
#'
#' @param selected_clusts A named numeric vector containing the selection
#' proportions for the selected clusters. The name of each entry is the name of
#' the corresponding cluster.
#' @param selected_feats A named integer vector; the indices of the features
#' with nonzero weights from all of the selected clusters.
#' @param n_clusters Integer; the number of clusters in the data (upper bound
#' for the length of selected_clusts)
#' @param p Integer; number of features in the data (all selected_feats should
#' be in 1:p)
#' @author Gregory Faletto, Jacob Bien
checkGetSelectedClustersOutput <- function(selected_clusts, selected_feats,
    n_clusters, p){
    stopifnot(is.numeric(selected_clusts))
    stopifnot(all(selected_clusts >= 0))
    stopifnot(all(selected_clusts <= 1))
    stopifnot(length(selected_clusts) >= 1)
    stopifnot(length(selected_clusts) <= n_clusters)
    stopifnot(length(names(selected_clusts)) ==
        length(unique(names(selected_clusts))))
    stopifnot(!is.null(names(selected_clusts)))
    stopifnot(all(!is.na(names(selected_clusts)) &
        names(selected_clusts) != ""))
    stopifnot(is.integer(selected_feats))
    stopifnot(length(selected_feats) == length(unique(selected_feats)))
    stopifnot(all(selected_feats %in% 1:p))
    stopifnot(length(selected_clusts) <= length(selected_feats))
}

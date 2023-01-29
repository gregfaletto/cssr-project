# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that the argument min_num_clusts to several 
#' functions is as expected
#'
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.)
#' @param p The number of features; since this is an upper bound on the number
#' of clusters of features, it is also an upper bound on min_num_clusts.
#' @param n_clusters The number of clusters; note that this is an upper bound
#' on min_num_clusts
#' @author Gregory Faletto, Jacob Bien
checkMinNumClusts <- function(min_num_clusts, p, n_clusters){
    stopifnot(length(min_num_clusts) == 1)
    stopifnot(is.numeric(min_num_clusts) | is.integer(min_num_clusts))
    stopifnot(!is.na(min_num_clusts))
    stopifnot(min_num_clusts == round(min_num_clusts))
    stopifnot(min_num_clusts >= 1)
    stopifnot(min_num_clusts <= p)
    stopifnot(min_num_clusts <= n_clusters)
}

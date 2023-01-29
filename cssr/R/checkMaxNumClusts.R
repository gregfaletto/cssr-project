# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that the argument max_num_clusts to several 
#' functions is as expected
#'
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) Can be NA, in which case
#' max_num_clusts will be ignored.
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.) max_num_clusts must be at least as
#' large as min_num_clusts.
#' @param p The number of features; since this is an upper bound on the number
#' of clusters of features, it is also an upper bound on max_num_clusts.
#' @param n_clusters The number of clusters; note that this is an upper bound
#' on max_num_clusts
#' @return The provided max_num_clusts, coerced to an integer if needed, and
#' coerced to be less than or equal to the total number of clusters.
#' @author Gregory Faletto, Jacob Bien
checkMaxNumClusts <- function(max_num_clusts, min_num_clusts, p, n_clusters){
    stopifnot(length(max_num_clusts) == 1)
    if(!is.na(max_num_clusts)){
        stopifnot(is.numeric(max_num_clusts) | is.integer(max_num_clusts))
        stopifnot(max_num_clusts == round(max_num_clusts))
        stopifnot(max_num_clusts >= 1)
        stopifnot(max_num_clusts <= p)
        max_num_clusts <- as.integer(min(n_clusters, max_num_clusts))
        stopifnot(max_num_clusts >= min_num_clusts)
    }
    return(max_num_clusts)
}

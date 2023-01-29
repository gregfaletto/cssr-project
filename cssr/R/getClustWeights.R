# Generated from _main.Rmd: do not edit by hand

#' Calculate weights for members of a cluster using selection proportions
#'
#' Given a cluster of features, the selection proportions for each cluster
#' member, and a specified weighting scheme, calculate the appropriate weights
#' for the cluster.
#' @param cluster_i An integer vector containing the indices of the members
#' of a cluster.
#' @param weighting Character; determines how to calculate the weights for
#' individual features within the selected clusters. Only those features with
#' nonzero weight within the selected clusters will be returned. Must be one of
#' "sparse", "weighted_avg", or "simple_avg'. For "sparse", all the weight is
#' put on the most frequently
#' selected individual cluster member (or divided equally among all the clusters
#' that are tied for the top selection proportion if there is a tie). For
#' "weighted_avg", only the features within a selected cluster that were
#' themselves selected on at least one subsample will have nonzero weight. For
#' "simple_avg", each cluster member gets equal weight regardless of the
#' individual feature selection proportions (that is, all cluster members within
#' each selected cluster will be returned.). See Faletto and Bien (2022) for
#' details.
#' @param feat_sel_props A numeric vector of selection proportions corresponding
#' to each of the p features.
#' @return A numeric vector of the same length as cluster_i containing the
#' weights corresponding to each of the features in cluster_i. The weights
#' will all be nonnegative and sum to 1.
#' @author Gregory Faletto, Jacob Bien
getClustWeights <- function(cluster_i, weighting, feat_sel_props){

    stopifnot(is.integer(cluster_i) | is.numeric(cluster_i))
    stopifnot(all(cluster_i == round(cluster_i)))
    n_weights <- length(cluster_i)
    stopifnot(length(unique(cluster_i)) == n_weights)

    p <- length(feat_sel_props)
    stopifnot(all(cluster_i %in% 1:p))

    # Get the selection proportions of each cluster member
    sel_props <- feat_sel_props[cluster_i]

    stopifnot(all(sel_props >= 0))
    stopifnot(all(sel_props <= 1))

    weights_i <- rep(as.numeric(NA), n_weights)

    # Weighted or simple average?
    if(weighting == "sparse"){
        # Sparse cluster stability selection: All features in cluster with
        # selection proportion equal to the max
        # for the cluster get equal weight; rest of cluster gets 0 weight
        if(sum(sel_props) == 0){
            weights_i <- rep(1/n_weights, n_weights)
        } else{
            maxes <- sel_props==max(sel_props)

            stopifnot(sum(maxes) > 0)
            stopifnot(sum(maxes) <= n_weights)

            weights_i <- rep(0, n_weights)
            weights_i[maxes] <- 1/sum(maxes)
        }
    } else if(weighting == "weighted_avg"){
        # Get weights for weighted average
        if(sum(sel_props) == 0){
            weights_i <- rep(1/n_weights, n_weights)
        } else{
            weights_i <- sel_props/sum(sel_props)
        }
    } else if(weighting == "simple_avg"){
        weights_i <- rep(1/n_weights, n_weights)
    } else{
        stop("weighting must be one of sparse, simple_avg, or weighted_avg")
    }

    stopifnot(abs(sum(weights_i) - 1) < 10^(-6))
    stopifnot(length(weights_i) == n_weights)
    stopifnot(length(weights_i) >= 1)
    stopifnot(all(weights_i >= 0))
    stopifnot(all(weights_i <= 1))

    return(weights_i)
}

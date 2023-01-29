# Generated from _main.Rmd: do not edit by hand

#' Calculate weights for each cluster member of all of the selected clusters.
#' 
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @param sel_clusters A named numeric vector containing the selection
#' proportions for the selected clusters. The name of each entry is the name
#' of the corresponding cluster.
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
#' @return A named list of the same length as sel_clusters of numeric vectors.
#' weights[[j]] is the weights to use for the jth selected cluster, and it has
#' the same name as the cluster it corresponds to.
#' @author Gregory Faletto, Jacob Bien
getAllClustWeights <- function(css_results, sel_clusters, weighting){

    # Check inputs
    stopifnot(class(css_results) == "cssr")

    stopifnot(is.numeric(sel_clusters))
    p_ret <- length(sel_clusters)
    stopifnot(length(unique(names(sel_clusters))) == p_ret)
    stopifnot(p_ret > 0)

    checkWeighting(weighting)

    # Get selection proportions and clusters
    feat_sel_props <- colMeans(css_results$feat_sel_mat)

    p <- length(feat_sel_props)
    stopifnot(p >= p_ret)

    clusters <- css_results$clusters
    stopifnot(all(names(sel_clusters) %in% names(clusters)))

    # Identify weights
    weights <- list()

    for(j in 1:p_ret){
        # Find the members of the cluster feature j is a member of
        cluster_j <- clusters[[names(sel_clusters)[j]]]
        # Get the weights for this cluster and add them to the list
        weights[[j]] <- getClustWeights(cluster_j, weighting, feat_sel_props)
    }

    # Add names to weights
    names(weights) <- names(sel_clusters)

    # Check output

    stopifnot(length(weights) == p_ret)
    stopifnot(is.list(weights))

    for(i in 1:p_ret){
        stopifnot(length(clusters[[names(sel_clusters)[i]]]) ==
            length(weights[[i]]))
        stopifnot(all(weights[[i]] >= 0))
        stopifnot(all(weights[[i]] <= 1))
        stopifnot(abs(sum(weights[[i]]) - 1) < 10^(-6))
    }
    return(weights)
}

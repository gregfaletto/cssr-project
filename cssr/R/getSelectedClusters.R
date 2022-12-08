# Generated from create-cssr.Rmd: do not edit by hand

#' From css output, obtain names of selected clusters and selection proportions,
#' indices of all selected features, and weights of individual cluster members
#'
#' If cutoff is too high for at least min_num_clusts clusters to be selected,
#' then it will be lowered until min_num_clusts can be selected. After that, if
#' the cutoff is too low such that more than max_num_clusts are selected, then
#' the cutoff will be increased until no more than max_num_clusts are selected.
#' Note that because clusters can have tied selection proportions, it is
#' possible that the number of selected clusters will be strictly lower than
#' max_num_clusts or strictly greater than min_num_clusts. In fact, it is
#' possible that both cutoffs won't be able to be satisfied simulteaneously,
#' even if there is a strictly positive difference between max_num_clusts and
#' min_num_clusts. If this occurs, max_num_clusts will take precedence over
#' min_num_clusts. getSelectedClusters will throw an error if the provided
#' inputs don't allow it to select any clusters. 
#' 
#' @param css_results An object of class "cssr" (the output of the function
#' css).
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
#' @param cutoff Numeric; getCssSelections will select and return only of those
#' clusters with selection proportions equal to at least cutoff. Must be between
#' 0 and 1.
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.)
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) If NA, max_num_clusts is ignored.
#' @return A named list with the following elements: \item{selected_clusts}{A
#' named numeric vector containing the selection proportions for the selected
#' clusters. The name of each entry is the name of the corresponding cluster.}
#' \item{selected_feats}{A named integer vector; the indices of the features
#' with nonzero weights from all of the selected clusters.} \item{weights}{A
#' named list of the same length as the number of selected clusters. Each list
#' element weights[[j]] is a numeric  vector of the weights to use for the jth
#' selected cluster, and it has the same name as the cluster it corresponds
#' to.}
#' @author Gregory Faletto, Jacob Bien
getSelectedClusters <- function(css_results, weighting, cutoff, min_num_clusts,
    max_num_clusts){
    # Check input
    stopifnot(class(css_results) == "cssr")

    # Eliminate clusters with selection proportions below cutoff
    clus_sel_props <- colMeans(css_results$clus_sel_mat)

    # Get selected clusters
    selected_clusts <- clus_sel_props[clus_sel_props >= cutoff]
    B <- nrow(css_results$feat_sel_mat)

    # Check that selected_clusts has length at least min_num_clusts
    while(length(selected_clusts) < min_num_clusts){
        cutoff <- cutoff - 1/B
        selected_clusts <- clus_sel_props[clus_sel_props >= cutoff]
    }

    # Check that selected_clusts has length at most max_num_clusts
    if(!is.na(max_num_clusts)){
        n_clusters <- ncol(css_results$clus_sel_mat)
        while(length(selected_clusts) > max_num_clusts){
            cutoff <- cutoff + 1/B
            if(cutoff > 1){
                break
            }
            # Make sure we don't reduce to a selected set of size 0
            if(any(clus_sel_props >= cutoff)){
                selected_clusts <- clus_sel_props[clus_sel_props >= cutoff]
            } else{
                break
            }
        }
    }

    clust_names <- names(selected_clusts)

    n_sel_clusts <- length(selected_clusts)

    # Check that n_sel_clusts is as expected, and throw warnings or an error if
    # not
    checkSelectedClusters(n_sel_clusts, min_num_clusts, max_num_clusts,
        max(clus_sel_props))
    
    ### Get selected features from selected clusters
    clusters <- css_results$clusters
    stopifnot(all(clust_names %in% names(clusters)))

    # Get a list of weights for all of the selected clusters
    weights <- getAllClustWeights(css_results, selected_clusts, weighting)

    # Get selected features from each cluster (those features with nonzero
    # weights)
    selected_feats <- integer()
    for(i in 1:n_sel_clusts){
        clus_i_name <- clust_names[i]
        clust_i <- clusters[[clus_i_name]]
        weights_i <- weights[[i]]
        selected_feats <- c(selected_feats, clust_i[weights_i != 0])
    }

    feat_names <- colnames(css_results$feat_sel_mat)

    names(selected_feats) <- feat_names[selected_feats]

    # Check output (already checked weights wihin getAllClustWeights)

    checkGetSelectedClustersOutput(selected_clusts, selected_feats,
        n_clusters=length(clusters), p=ncol(css_results$feat_sel_mat))

    return(list(selected_clusts=selected_clusts,
        selected_feats=selected_feats, weights=weights))
}

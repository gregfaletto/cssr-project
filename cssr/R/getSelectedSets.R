# Generated from _main.Rmd: do not edit by hand

#' Converts a selected set from X_glmnet to selected sets and selected clusters
#' from the original feature space of X.
#'
#' @param lasso_set A vector containing the indices of selected cluster
#' representatives or prototypes.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters is
#' equal to the number of clusters.) All identified clusters must be
#' non-overlapping. All features appear in exactly one cluster (any unclustered
#' features must be in their own "cluster" of size 1).
#' @param prototypes An integer vector whose length must be equal to the number
#' of clusters. Entry i should be the index of the feature belonging to cluster
#' i that is most highly correlated with y (that is, the prototype for the
#' cluster, as in the protolasso).
#' @param feat_names Character vector; the names of the features in X.
#' @return A list containing two items: \item{selected_set}{An integer vector
#' with length equal to lasso_set containing a set of selected features in the
#' original X matrix. (Selections in lasso_set corresponding to a cluster will
#' be replaced by the cluster's prototype from X.)}
#' \item{selected_clusts_list}{A named list of integer vectors with length equal
#' to selected_set. selected_clusts_list[[k]] will be an integer vector
#' containing the indices of the features in X that are in the cluster
#' containing prototype selected_set[k].}
#' @author Gregory Faletto, Jacob Bien
getSelectedSets <- function(lasso_set, clusters, prototypes, feat_names){
    
    model_size <- length(lasso_set)
    stopifnot(model_size > 0)

    stopifnot(length(unique(lasso_set)) == model_size)
    stopifnot(all(lasso_set <= length(clusters)))

    selected_set <- integer()
    selected_clusts_list <- list()
    # Recover features from original feature space
    for(k in 1:model_size){
        selected_cluster_k <- clusters[[lasso_set[k]]]
        stopifnot(is.integer(selected_cluster_k))
        selected_clusts_list[[k]] <- selected_cluster_k

        if(length(selected_cluster_k) == 1){
            stopifnot(!(selected_cluster_k %in% selected_set))
            selected_set <- c(selected_set, selected_cluster_k)
        } else{
            sel_prototype <- which(prototypes %in% selected_cluster_k)
            stopifnot(length(sel_prototype) == 1)
            stopifnot(!(prototypes[sel_prototype] %in% selected_set))
            selected_set <- c(selected_set, prototypes[sel_prototype])
        }
    }

    stopifnot(length(selected_set) == model_size)
    stopifnot(length(unique(selected_set)) == model_size)
    
    if(any(!is.na(feat_names))){
        names(selected_set) <- feat_names[selected_set]
    }

    stopifnot(length(selected_clusts_list) == model_size)
    all_feats <- unlist(selected_clusts_list)
    stopifnot(length(all_feats) == length(unique(all_feats)))

    return(list(selected_set=selected_set,
        selected_clusts_list=selected_clusts_list))
}

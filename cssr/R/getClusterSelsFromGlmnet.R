# Generated from _main.Rmd: do not edit by hand

#' Extracts selected clusters and cluster prototypes from the glmnet lasso
#' output
#'
#' @param lasso_sets A list of integer vectors. Each vector represents a set of
#' features selected by the lasso for a given value of the penalty parameter
#' lambda.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters is
#' equal to the number of clusters.) All identified clusters must be
#' non-overlapping. All features appear in exactly one cluster (any unclustered
#' features must be in their own "cluster" of size 1).
#' @param prototypes An integer vector whose length must be equal to the number
#' of clusters. Entry i should be the index of the feature belonging to cluster
#' i that is most highly correlated with y (that is, the prototype for the
#' cluster, as in the protolasso; see Reid and Tibshirani 2016).
#' @param feat_names Character vector; the names of the features in X. (If the
#' X provided to protolasso or clusterRepLasso did not have feature names,
#' feat_names will be NA.)
#' @return A list containing the following items: \item{selected_sets}{A list of
#' integer vectors. Entry k of this list contains a selected set of size k
#' yielded by glmnet--each member of the set is the index of a single feature
#' from a cluster selected by either the protolasso or the cluster
#' representative lasso (the prototype from that cluster--the cluster member
#' most highly correlated with y). (If no set of size k was selected, entry k
#' will be empty.)} \item{selected_clusts_list}{A list of lists; entry k of this
#' list is a list of length k of clusters (the clusters that were selected by
#' the cluster representative lasso).}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}. \cr Bühlmann, P.,
#' Rütimann, P., van de Geer, S., & Zhang, C. H. (2013). Correlated variables in
#' regression: Clustering and sparse estimation.
#' \emph{Journal of Statistical Planning and Inference}, 143(11), 1835–1858.
#' \url{https://doi.org/10.1016/j.jspi.2013.05.019}.
getClusterSelsFromGlmnet <- function(lasso_sets, clusters, prototypes,
    feat_names){

    if(any(!is.na(feat_names))){
        stopifnot(all(!is.na(feat_names)))
    }

    # Largest selected set among all those in lasso_sets
    max_length <- max(vapply(lasso_sets, length, integer(1)))

    # Preparing lists to store 
    selected_sets <- list()
    selected_clusts_list <- list()
    
    for(j in 1:max_length){
        # Lasso selected set of size j
        lasso_sets_j <- lasso_sets[lapply(lasso_sets, length) == j]
        # Are there any lasso selected sets of size j? (If not, we will skip to
        # the next j, and slot j in the list will be empty.)
        if(length(lasso_sets_j) > 0){

            # Select the first set of size j
            lasso_set_j <- lasso_sets_j[[1]]
            stopifnot(length(lasso_set_j) == j)
            
            ret <- getSelectedSets(lasso_set=lasso_set_j, clusters=clusters,
                prototypes=prototypes, feat_names=feat_names)

            selected_sets[[j]] <- ret$selected_set
            selected_clusts_list[[j]] <- ret$selected_clusts_list

            rm(ret)
        }
    }

    stopifnot(length(selected_sets) <= max_length)
    stopifnot(length(selected_clusts_list) <= max_length)

    return(list(selected_sets=selected_sets,
        selected_clusts_list=selected_clusts_list))
}

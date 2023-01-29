# Generated from _main.Rmd: do not edit by hand

#' Converts the provided design matrix to an appropriate format for either the
#' protolasso or the cluster representative lasso.
#'
#' Creates design matrix for glmnet by dealing with clusters (for
#' type="protolasso", discards all cluster members except prototype; for
#' type="clusterRepLasso", replaces all cluster members with a simple
#' average of all the cluster members).
#' @param x A numeric matrix; the provided matrix with n observations and p
#' features.
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters should
#' be equal to the number of clusters.) All identified clusters should be
#' non-overlapping. All features should appear in exactly one cluster (any
#' unclustered features should be put in their own "cluster" of size 1).
#' @param type Character; "protolasso" for the protolasso or "clusterRepLasso"
#' for the cluster representative lasso.
#' @param prototypes Only required for type "protolasso". An integer vector
#' whose length is equal to the number of clusters. Entry i should be the
#' prototype for cluster i (the feature belonging to cluster i that is most
#' highly correlated with y; see Reid and Tibshirani 2016).
#' @return A numeric matrix; the design matrix as required for the protolasso or
#' cluster representative lasso, prepared for input to glmnet.
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
getXglmnet <- function(x, clusters, type, prototypes=NA){
    
    # Check inputs
    checkGetXglmnetInputs(x, clusters, type, prototypes)

    n <- nrow(x)
    p <- ncol(x)

    # if(n_clusters > 0){
    for(i in 1:length(clusters)){
        cluster_i <- clusters[[i]]
        
        if(type == "protolasso"){
            if(length(cluster_i) > 1){
                prototype_ind_i <- which(prototypes %in% cluster_i)
                stopifnot(length(prototype_ind_i) == 1)
                prototype_i <- prototypes[prototype_ind_i]
            } else{
                prototype_i <- cluster_i
            }
            
            X_glmnet_i <- x[, prototype_i]
        } else {
            stopifnot(type == "clusterRepLasso")
            if(length(cluster_i) > 1){
                X_glmnet_i <- rowMeans(x[, cluster_i])
            } else{
                X_glmnet_i <- x[, cluster_i]
            }    
        }

        stopifnot(length(X_glmnet_i) == n)
        
        if(i == 1){
            X_glmnet <- as.matrix(X_glmnet_i)
        } else{
            X_glmnet <- cbind(X_glmnet, X_glmnet_i)
        }
    }
    stopifnot(ncol(X_glmnet) == length(clusters))
    colnames(X_glmnet) <- character()

    # Check output
    stopifnot(is.matrix(X_glmnet))
    stopifnot(nrow(X_glmnet) == n)
    stopifnot(ncol(X_glmnet) <= p)
    stopifnot(ncol(X_glmnet) >= 1)
    
    return(X_glmnet)
}

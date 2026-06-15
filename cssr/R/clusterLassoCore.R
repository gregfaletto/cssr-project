# Generated from _main.Rmd: do not edit by hand

#' Shared implementation of protolasso and clusterRepLasso
#'
#' Internal helper holding the common body of `protolasso()` and
#' `clusterRepLasso()`, which differ only in the `type` passed to `getXglmnet()`
#' (the sole point where the two procedures diverge: protolasso discards
#' non-prototype cluster members, clusterRepLasso replaces each cluster with its
#' representative).
#' @param X,y,clusters,nlambda As documented in `protolasso()` /
#' `clusterRepLasso()`.
#' @param type Character; either "protolasso" or "clusterRepLasso", passed to
#' `getXglmnet()` to select the design-matrix construction.
#' @return As documented in `protolasso()` / `clusterRepLasso()`.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
clusterLassoCore <- function(X, y, clusters, nlambda, type){

    # Handle and format inputs; get cluster prototypes
    ret <- processClusterLassoInputs(X, y, clusters, nlambda)

    x <- ret$x
    clusters <- ret$clusters
    prototypes <- ret$prototypes
    feat_names <- ret$var_names

    rm(ret)

    # Format the design matrix for glmnet according to the chosen procedure
    # (type = "protolasso" or "clusterRepLasso"); see getXglmnet().
    X_glmnet <- getXglmnet(x, clusters, type=type, prototypes=prototypes)

    # Estimate the lasso on the cluster prototypes / representatives
    fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=nlambda)
    lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

    # Obtain a tidy list of selected sets--one for each model size
    cluster_sel_results <- getClusterSelsFromGlmnet(lasso_sets, clusters,
        prototypes, feat_names)

    return(list(selected_sets=cluster_sel_results$selected_sets,
        selected_clusts_list=cluster_sel_results$selected_clusts_list,
        beta=fit$beta))
}

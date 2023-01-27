# Generated from create-cssr.Rmd: do not edit by hand

#' Select features via the protolasso (Reid and Tibshirani 2016)
#'
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' p >= 2 features/predictors
#' @param y The response; A length n numeric (or integer) real-valued vector.
#' @param clusters A list of integer vectors; each vector should contain the 
#' indices of a cluster of features (a subset of 1:p). (If there is only one
#' cluster, clusters can either be a list of length 1 or an integer vector.)
#' All of the provided clusters must be non-overlapping. Every feature not
#' appearing in any cluster will be assumed to be unclustered (that is, they
#' will be treated as if they are in a "cluster" containing only themselves).
#' Default is list() (so no clusters are specified).
#' @param nlambda Integer; the number of lambda values to use in the lasso fit
#' for the protolasso. Default is 100 (following the default for glmnet). For
#' now, nlambda must be at least 2 (using a single lambda is not supported).
#' @return A list with three elements. \item{selected_sets}{A list of integer
#' vectors. Entry k of this list contains a selected set (an integer vector) of
#' size k yielded by the protolasso (If no set of size k was selected, entry k
#' will be empty.)} \item{selected_clusts_list}{A list; each element of the list
#' is a named list of selected clusters. (That is, if a selected set of size k
#' was yielded by the protolasso, then selected_clusts_list[[k]] is a named
#' list of length k, where each member of the list is an integer vector
#' of cluster members. In particular, selected_clusts_lists[[k]][[j]] will be
#' the cluster that contains feature selected_sets[[k]][j].)} \item{beta}{The
#' beta output from glmnet when the lasso was estimated on a matrix of
#' prototypes. (See documentation for the function glmnet from the glmnet
#' package for details.)}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
#' @export
protolasso <- function(X, y, clusters, nlambda=100){

    # Handle and format inputs; get cluster prototypes
    ret <- processClusterLassoInputs(X, y, clusters, nlambda)

    x <- ret$x
    clusters <- ret$clusters
    prototypes <- ret$prototypes
    feat_names <- ret$var_names

    rm(ret)

    # Format the design matrix for glmnet according to the protolasso procedure
    X_glmnet <- getXglmnet(x, clusters, type="protolasso",
        prototypes=prototypes)

    # Estimate the lasso on the cluster prototypes
    fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=nlambda)
    lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

    # Finally, obtain a tidy list of selected sets--one for each model size
    cluster_sel_results <- getClusterSelsFromGlmnet(lasso_sets, clusters,
        prototypes, feat_names)

    return(list(selected_sets=cluster_sel_results$selected_sets,
        selected_clusts_list=cluster_sel_results$selected_clusts_list,
        beta=fit$beta))
}

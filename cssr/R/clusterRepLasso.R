# Generated from _main.Rmd: do not edit by hand

#' Select features via the cluster representative lasso (Bühlmann et. al. 2013)
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
#' CAUTION: if the provided X is a data.frame that contains a categorical
#' feature with more than two levels, then the resulting matrix made from
#' model.matrix will have a different number of columns than the provided
#' data.frame, some of the feature numbers will change, and the clusters
#' argument will not work properly (in the current version of the package).
#' To get correct results in this case, please use model.matrix to convert
#' the data.frame to a numeric matrix on your own, then provide this matrix
#' and cluster assignments with respect to this matrix. Default is list() (so no
#' clusters are specified).
#' @param nlambda Integer; the number of lambda values to use in the lasso fit
#' for the cluster representative lasso. Default is 100 (following the default
#' for glmnet). For now, nlambda must be at least 2 (using a single lambda is
#' not supported).
#' @return A list with three elements. \item{selected_sets}{A list of integer
#' vectors. Entry k of this list contains a selected set (an integer vector) of
#' size k yielded by the lasso--each member of the set is the index of a single
#' feature from a cluster selected by the cluster representative lasso (the
#' prototype from that cluster--the cluster member most highly correlated with
#' y). (If no set of size k was selected, entry k will be empty.)}
#' \item{selected_clusts_list}{A list; each element of the list is a named list
#' of selected clusters. (That is, if a selected set of size k was yielded by
#' the cluster representative lasso, then selected_clusts_list[[k]] is a named
#' list of length k, where each member of the list is an integer vector
#' of cluster members. Note that selected_clusts_lists[[k]][[j]] will be the
#' cluster that contains feature selected_sets[[k]][j].)} \item{beta}{The beta
#' output from glmnet when the lasso was estimated on a matrix of prototypes.
#' (See documentation for the function glmnet from the glmnet package for
#' details.)}
#' @references Bühlmann, P., Rütimann, P., van de Geer, S., & Zhang, C. H.
#' (2013). Correlated variables in regression: Clustering and sparse estimation.
#' \emph{Journal of Statistical Planning and Inference}, 143(11), 1835–1858.
#' \url{https://doi.org/10.1016/j.jspi.2013.05.019}. \cr Jerome Friedman, Trevor
#' Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear
#' Models via Coordinate Descent. \emph{Journal of Statistical Software}, 33(1)
#' ' 1-22. URL \url{https://www.jstatsoft.org/v33/i01/}.
clusterRepLasso <- function(X, y, clusters=list(), nlambda=100){

    # Handle and format inputs; get cluster prototypes
    ret <- processClusterLassoInputs(X, y, clusters, nlambda)

    x <- ret$x
    clusters <- ret$clusters
    prototypes <- ret$prototypes
    feat_names <- ret$var_names

    rm(ret)

    # Format the design matrix for glmnet according to the cluster
    # representative lasso procedure
    X_glmnet <- getXglmnet(x, clusters, type="clusterRepLasso",
        prototypes=prototypes)

    # Estimate the lasso on the cluster representatives
    fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=nlambda)
    lasso_sets <- unique(glmnet::predict.glmnet(fit, type="nonzero"))

    # Finally, extract the desired information from the lasso fit--all the
    # sets of selected clusters (one for each observed model size), and
    # corresponding sets of selected features
    cluster_sel_results <- getClusterSelsFromGlmnet(lasso_sets, clusters,
        prototypes, feat_names)

    return(list(selected_sets=cluster_sel_results$selected_sets,
        selected_clusts_list=cluster_sel_results$selected_clusts_list,
        beta=fit$beta))
}

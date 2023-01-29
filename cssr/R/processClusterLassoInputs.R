# Generated from _main.Rmd: do not edit by hand

#' Check the inputs to protolasso and clusterRepLasso, format clusters, and
#' identify prototypes for each cluster
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
#' @return A list with four elements. \item{x}{The provided X, converted to a
#' matrix if it was provided as a data.frame, and with column names removed.}
#' \item{clusters}{A named list where each entry is an integer vector of indices
#' of features that are in a common cluster. (The length of list clusters is
#' equal to the number of clusters.) All identified clusters are
#' non-overlapping. All features appear in exactly one cluster (any unclustered
#' features will be put in their own "cluster" of size 1).}
#' \item{prototypes}{An integer vector whose length is equal to the number of
#' clusters. Entry i is the index of the feature belonging to cluster i that is
#' most highly correlated with y (that is, the prototype for the cluster, as in
#' the protolasso; see Reid and Tibshirani 2016).} \item{var_names}{If the
#' provided X matrix had column names, the names of the featurrs in the provided
#' X matrix. If no names were provided, feat_names will be NA.}
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
processClusterLassoInputs <- function(X, y, clusters, nlambda){

    stopifnot(is.matrix(X) | is.data.frame(X))

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X)){
        X <- stats::model.matrix(~ ., X)
        X <- X[, colnames(X) != "(Intercept)"]
    }

    stopifnot(is.matrix(X))
    stopifnot(all(!is.na(X)))

    feat_names <- as.character(NA)
    if(!is.null(colnames(X))){
        feat_names <- colnames(X)
        if(any(is.na(feat_names))){
            stop("Some features in provided X matrix had valid names and some had NA names; please neither name all features in X or remove the names altogether.")
        }
    }

    n <- nrow(X)

    colnames(X) <- character()

    stopifnot(is.numeric(y) | is.integer(y))
    stopifnot(n == length(y))
    stopifnot(all(!is.na(y)))

    # Check clusters argument
    clusters <- checkCssClustersInput(clusters)

    # Format clusters into a list where all features are in exactly one
    # cluster (any unclustered features are put in their own "cluster" of size
    # 1).
    clust_names <- as.character(NA)
    if(!is.null(names(clusters)) & is.list(clusters)){
        clust_names <- names(clusters)
    }

    cluster_results <- formatClusters(clusters, p=ncol(X),
        clust_names=clust_names, get_prototypes=TRUE, x=X, y=y)

    clusters <- cluster_results$clusters
    prototypes <- cluster_results$prototypes

    rm(cluster_results)

    stopifnot(length(clusters) == length(prototypes))

    stopifnot(is.numeric(nlambda) | is.integer(nlambda))
    stopifnot(length(nlambda) == 1)
    stopifnot(!is.na(nlambda))
    stopifnot(nlambda >= 2)

    return(list(x=X, clusters=clusters, prototypes=prototypes,
        var_names=feat_names))
}

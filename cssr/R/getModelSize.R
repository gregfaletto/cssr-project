# Generated from _main.Rmd: do not edit by hand

#' Automated estimation of model size
#'
#' This function is uses the lasso with cross-validation to estimate the
#' model size. Before using the lasso, in each cluster all features will be
#' dropped from X except the one feature with the highest marginal correlation
#' with y, as in the protolasso (Reid and Tibshirani 2016).
#' 
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' the p >= 2 features/predictors.
#' @param y A length-n numeric vector containing the responses; `y[i]` is the
#' response corresponding to observation `X[i, ]`. (Note that for the css
#' function, y does not have to be a numeric response, but for this function,
#' the underlying selection procedure is the lasso, so y must be a real-valued
#' response.)
#' @param clusters A named list where each entry is an integer vector of indices
#' of features that are in a common cluster, as in the output of css.
#' (The length of list clusters is equal to the number of clusters.) All
#' identified clusters must be non-overlapping, and all features must appear in
#' exactly one cluster (any unclustered features should be in their own
#' "cluster" of size 1).
#' @return An integer; the estimated size of the model. The minimum returned
#' value is 1, even if the lasso with cross-validation chose no features.
#' @author Gregory Faletto, Jacob Bien
#' @references Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
#' testing using cluster prototypes. \emph{Biostatistics}, 17(2), 364–376.
#' \url{https://doi.org/10.1093/biostatistics/kxv049}.
#' @export
getModelSize <- function(X, y, clusters){
    stopifnot(is.matrix(X) | is.data.frame(X))

    # Check if x is a matrix; if it's a data.frame, convert to matrix.
    if(is.data.frame(X)){
        X <- stats::model.matrix(~ ., X)
        X <- X[, colnames(X) != "(Intercept)"]
    }

    stopifnot(is.matrix(X))
    stopifnot(all(!is.na(X)))
    stopifnot(is.numeric(X) | is.integer(X))
    n <- nrow(X)

    # Since the model size will be determined by cross-validation, the provided
    # y must be real-valued (this should be checked internally in other
    # functions before getModelSize is called, but this check is here just in
    # case).
    if(!is.numeric(y) & !is.integer(y)){
        stop("getModelSize is trying to determine max_num_clusts using the lasso with cross-validation, but the y provided to getModelSize was not real-valued.")
    }
    stopifnot(length(y) == n)

    # Check clusters argument
    clusters <- checkCssClustersInput(clusters)

    ### Format clusters into a list where all features are in exactly one
    # cluster (any unclustered features are put in their own "cluster" of size
    # 1).
    clust_names <- as.character(NA)
    if(!is.null(names(clusters)) & is.list(clusters)){
        clust_names <- names(clusters)
    }

    clusters <- formatClusters(clusters, p=ncol(X),
        clust_names=clust_names)$clusters

    X_size <- X

    if(length(clusters) > 0){
        # Create modified design matrix by dropping all but one feature from
        # each cluster
        feats_to_drop <- integer()
        for(i in 1:length(clusters)){
            cluster_i <- clusters[[i]]
            if(length(cluster_i) > 1){
                feat_to_keep <- identifyPrototype(cluster_i, X, y)
                feats_to_drop <- c(feats_to_drop, setdiff(cluster_i,
                    feat_to_keep))
            }
        }
        if(length(feats_to_drop) > 0){
            X_size <- X_size[, -1*feats_to_drop]
        }
    }

    size_results <- glmnet::cv.glmnet(x=X_size, y=y, family="gaussian")
    coefs <- as.numeric(glmnet::coef.glmnet(size_results, s="lambda.1se"))

    # Number of nonzero coefficients (subtract one in order to ignore intercept)
    size <- length(coefs[coefs != 0]) - 1

    # Check output
    stopifnot(is.numeric(size) | is.integer(size))
    stopifnot(!is.na(size))
    stopifnot(length(size) == 1)
    stopifnot(size == round(size))

    return(as.integer(max(size, 1)))
}

# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that inputs to the function `css()` are as expected,
#' and modify inputs if needed
#'
#' @param X An n x p numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix) containing
#' p >= 2 features/predictors.
#' @param y The response; can be anything that takes the form of an
#' n-dimensional vector, with the ith entry corresponding to the ith row of X.
#' Typically (and for default fitfun = cssLasso), y should be an n-dimensional
#' numeric vector.
#' @param lambda A tuning parameter or set of tuning parameters that may be used
#' by the feature selection method fitfun. In the default case when
#' fitfun = cssLasso, lambda should be a numeric: the penalty to use for each
#' lasso fit. (css does not require lambda to be any particular object because
#' for a user-specified feature selection method fitfun, lambda can be an
#' arbitrary object. See the description of fitfun below.)
#' @param clusters A list of integer vectors; each vector should contain the 
#' indices of a cluster of features (a subset of 1:p). (If there is only one
#' cluster, clusters can either be a list of length 1 or an integer vector.)
#' All of the provided clusters must be non-overlapping. Every feature not
#' appearing in any cluster will be assumed to be unclustered (that is, they
#' will be treated as if they are in a "cluster" containing only themselves). If
#' clusters is a list of length 0 (or a list only containing clusters of length
#' 1), then css() returns the same results as stability selection (so the
#' returned feat_sel_mat will be identical to clus_sel_mat). Names for the
#' clusters will be needed later; any clusters that are not given names in the
#' provided list will be given names automatically by css. Default is list() (so
#' no clusters are specified).
#' @param fitfun A function; the feature selection function used on each
#' subsample by cluster stability selection. This can be any feature selection
#' method; the only requirement is that it accepts the arguments (and only the
#' arguments) X, y, and lambda and returns an integer vector that is a subset of
#' 1:p. For example, fitfun could be best subset selection or forward stepwise
#' selection or LARS and lambda could be the desired model size; or fitfun could be the
#' elastic net and lambda could be a length-two vector specifying lambda and
#' alpha. Default is cssLasso, an implementation of lasso (relying on the R
#' package glmnet), where lambda must be a positive numeric specifying the L1
#' penalty for the lasso.
#' @param sampling_type A character vector; either "SS" or "MB". For "MB",
#' all B subsamples are drawn randomly (as proposed by Meinshausen and Bühlmann
#' 2010). For "SS", in addition to these B subsamples, the B complementary pair
#' subsamples will be drawn as well (see Faletto and Bien 2022 or Shah and
#' Samworth 2013 for details). Default is "SS", and "MB" is not supported yet.
#' @param B Integer or numeric; the number of subsamples. Note: For
#' sampling.type=="MB" the total number of subsamples will be `B`; for
#' sampling_type="SS" the number of subsamples will be `2*B`. Default is 100
#' for sampling_type="MB" and 50 for sampling_type="SS".
#' @param prop_feats_remove Numeric; if prop_feats_remove is greater than 0,
#' then on each subsample, each feature is randomly dropped from the design
#' matrix that is provided to fitfun with probability prop_feats_remove
#' (independently across features). That is, in a typical subsample,
#' prop_feats_remove*p features will be dropped (though this number will vary).
#' This is similar in spirit (but distinct from) extended stability selection
#' (Beinrucker et. al. 2016); see their paper for some of the benefits of
#' dropping features (besides increasing computational speed and decreasing
#' memory requirements). For sampling_type="SS", the features dropped in
#' each complementary pair of subsamples are identical in order to ensure that
#' the theoretical guarantees of Faletto and Bien (2022) are retained within
#' each individual pair of subsamples. (Note that this feature is not
#' investigated either theoretically or in simulations by Faletto and Bien
#' 2022). Must be between 0 and 1. Default is 0.
#' @param train_inds Optional; an integer or numeric vector containing the
#' indices of observations in X and y to set aside for model training by the
#' function getCssPreds after feature selection. (This will only work if y is
#' real-valued, because getCssPreds using ordinary least squares regression to
#' generate predictions.) If train_inds is not provided, all of the observations
#' in the provided data set will be used for feature selection.
#' @param num_cores Optional; an integer. If using parallel processing, the
#' number of cores to use for parallel processing (num_cores will be supplied
#' internally as the mc.cores argument of parallel::mclapply).
#' @return A named list with the following elements: \item{feat_names}{A 
#' character vector containing the column names of X (if the provided X
#' had column names). If the provided X did not have column names, feat_names
#' will be NA.} \item{X}{The provided X, converted to a matrix if it was
#' originally provided as a data.frame, and with feature names removed if they
#' had been provided.}\item{clusters}{A list of integer vectors; each vector
#' will contain the indices of a cluster of features. Any duplicated clusters
#' provided in the input will be removed.}
#' @author Gregory Faletto, Jacob Bien
checkCssInputs <- function(X, y, lambda, clusters, fitfun, sampling_type, B,
    prop_feats_remove, train_inds, num_cores){

    stopifnot(is.matrix(X) | is.data.frame(X))

    clust_names <- as.character(NA)
    if(!is.null(names(clusters)) & is.list(clusters)){
        clust_names <- names(clusters)
    }

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
    }

    n <- nrow(X)
    p <- ncol(X)

    if(!is.null(colnames(X))){
        feat_names <- colnames(X)
    }

    stopifnot(p >= 2)
    if(length(feat_names) > 1){
        stopifnot(length(feat_names) == p)
    } else{
        stopifnot(is.na(feat_names))
    }

    colnames(X) <- character()

    stopifnot(length(y) == n)
    # Intentionally don't check y or lambda further to allow for flexibility--these
    # inputs should be checked within fitfun.

    # Check clusters argument
    clusters <- checkCssClustersInput(clusters)

    ### Format clusters into a list where all features are in exactly one
    # cluster (any unclustered features are put in their own "cluster" of size
    # 1).
    clusters <- formatClusters(clusters, p=p, clust_names=clust_names)$clusters

    stopifnot(class(fitfun) == "function")
    stopifnot(length(fitfun) == 1)
    if(!identical(formals(fitfun), formals(cssLasso))){
        err_mess <- paste("fitfun must accept arguments named X, y, and lambda. Detected arguments to fitfun:",
            paste(names(formals(fitfun)), collapse=", "))
        stop(err_mess)
    }

    checkSamplingType(sampling_type)
    checkB(B)
    checkPropFeatsRemove(prop_feats_remove, p)

    stopifnot(is.numeric(train_inds) | is.integer(train_inds))
    if(length(train_inds) > 0){
        stopifnot(all(!is.na(train_inds)))
        stopifnot(all(round(train_inds) == train_inds))
        stopifnot(length(train_inds) == length(unique(train_inds)))
        stopifnot(all(train_inds >= 1))
        stopifnot(all(train_inds) <= n)
        stopifnot(length(train_inds) <= n - 2)
        stopifnot(length(train_inds) >= 1)
    }

    stopifnot(length(num_cores) == 1)
    stopifnot(is.integer(num_cores) | is.numeric(num_cores))
    stopifnot(!is.na(num_cores))
    stopifnot(num_cores == round(num_cores))
    stopifnot(num_cores >= 1)
    stopifnot(num_cores <= parallel::detectCores())

    return(list(feat_names=feat_names, X=X, clusters=clusters))
}

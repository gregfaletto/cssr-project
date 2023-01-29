# Generated from _main.Rmd: do not edit by hand

#' Helper function run on each subsample
#' 
#' Runs provided feature selection method fitfun on each subsample for cluster
#' stability selection (this function is called within mclapply).
#' @param input Could be one of two things: \item{subsample}{An integer vector
#' of size `n/2` containing the indices of the observations in the subsample.}
#' \item{drop_var_input}{A named list containing two elements: one named
#' "subsample" and the same as the previous description, and a logical vector
#' named "feats_to_keep" containing the indices of the features to be
#' automatically selected.} (The first object is the output of the function
#' createSubsamples when the provided prop_feats_remove is 0, the default, and
#' the second object is the output of createSubsamples when prop_feats_remove >
#' 0.)
#' @param x an n x p numeric matrix containing the predictors. (This should be
#' the full design matrix provided to css.)
#' @param y A response; can be any response that takes the form of a length n
#' vector and is used (or not used) by fitfun. Typically (and for default fitfun
#' = cssLasso), y should be an n-dimensional numeric vector containing the
#' response. This should be the full response provided to css.
#' @param lambda A tuning parameter or set of tuning parameters that may be used
#' by the feature selection method. For example, in the default case when
#' fitfun = cssLasso, lambda is a numeric: the penalty to use for each lasso
#' fit.
#' @param fitfun A function that takes in arguments X, y, and lambda and returns
#' a vector of indices of the columns of X (selected features).
#' @return An integer vector; the indices of the features selected by fitfun.
#' @author Gregory Faletto, Jacob Bien
cssLoop <- function(input, x, y, lambda, fitfun){
    # Check inputs
    stopifnot(is.matrix(x))
    stopifnot(all(!is.na(x)))

    colnames(x) <- character()
    n <- nrow(x)
    p <- ncol(x)

    stopifnot(length(y) == n)
    stopifnot(!is.matrix(y))
    # Intentionally don't check y or lambda further to allow for flexibility--these
    # inputs should be checked within fitfun.

    if(!is.list(input)){
        subsample <- input
        feats_to_keep <- rep(TRUE, p)
    } else{
        stopifnot(all(names(input) == c("subsample", "feats_to_keep")))
        subsample <- input$subsample
        feats_to_keep <- input$feats_to_keep
    }

    stopifnot(is.integer(subsample))
    stopifnot(all(subsample == round(subsample)))
    stopifnot(floor(n/2) == length(subsample))
    stopifnot(length(subsample) == length(unique(subsample)))

    stopifnot(is.logical(feats_to_keep))
    stopifnot(length(feats_to_keep) == p)

    selected <- do.call(fitfun, list(X=x[subsample, feats_to_keep],
        y=y[subsample], lambda=lambda))

    selected <- which(feats_to_keep)[selected]

    # Check output
    checkCssLoopOutput(selected, p, as.integer(which(feats_to_keep)))

    return(as.integer(selected))
}

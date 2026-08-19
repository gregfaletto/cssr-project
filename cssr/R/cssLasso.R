# Generated from _main.Rmd: do not edit by hand

#' Provided fitfun implementing the lasso
#'
#' Function used to select features with the lasso on each subsample in cluster
#' stability selection. Uses glmnet implementation of the lasso.
#' @param X A design matrix containing the predictors. (In practice this will
#' be a subsample of the full design matrix provided to `css()`.)
#' @param y A numeric vector containing the response.
#' @param lambda Either a single nonnegative number for the lasso penalty to
#' use on each subsample (in which case a pure lasso fit, alpha = 1, is used),
#' or a named length-2 numeric vector `c(lambda = <value>, alpha = <value>)`
#' bundling the penalty together with the elastic net mixing parameter alpha
#' (which must be in `(0, 1]`); in the latter case an elastic net fit with that
#' alpha is used. (For now, only one lambda value can be provided to
#' `cssLasso()`; in the future, we plan to allow for multiple lambda values to be
#' provided to `cssLasso()`, as described in Faletto and Bien 2022.)
#' @return An integer vector; the indices of the features selected by the lasso.
#' @author Gregory Faletto, Jacob Bien
#' @references 
#' 
#' Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
#' 
#' Jerome Friedman, Trevor Hastie,
#' Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models
#' via Coordinate Descent. \emph{Journal of Statistical Software}, 33(1), 1-22.
#' URL \url{https://www.jstatsoft.org/v33/i01/}.
#' @examples
#' set.seed(1)
#' data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
#'   cluster_size = 4, n_clusters = 1, snr = 3)
#' # cssLasso is the default base feature-selection method used by css();
#' # it returns the integer indices selected at the given lambda.
#' selected <- cssLasso(X = data$X, y = data$y, lambda = 0.01)
#' selected
#' @export
cssLasso <- function(X, y, lambda){
    # Check inputs

    checkCssLassoInputs(X, y, lambda)

    n <- nrow(X)
    p <- ncol(X)

    # lambda may be either a single nonnegative number (pure lasso, as in the
    # original implementation) or a named length-2 numeric vector
    # c(lambda=<value>, alpha=<value>) bundling the elastic net mixing
    # parameter alpha alongside the penalty. Unpack alpha if it is provided.
    if(length(lambda) == 2){
        alpha <- unname(lambda["alpha"])
        lambda <- unname(lambda["lambda"])
    } else{
        alpha <- 1
    }

    # Fit a lasso path (full path for speed, per glmnet documentation)

    # Build the model with do.call so the stored call (lasso_model$call) carries
    # the literal numeric value of alpha rather than the symbol `alpha`. This is
    # load-bearing: predict.glmnet(..., exact=TRUE) below re-evaluates the
    # stored call via update(), and if that call contained the symbol `alpha`
    # (as glmnet::glmnet(X, y, family="gaussian", alpha=alpha) would store) the
    # refit would throw "object 'alpha' not found" -- on both the elastic-net
    # and the alpha=1 paths. See Decision Log / Surprises in the plan.
    lasso_model <- do.call(glmnet::glmnet,
        list(x=X, y=y, family="gaussian", alpha=alpha))
    stopifnot(all.equal(class(lasso_model), c("elnet", "glmnet")))

    # Get coefficients at desired lambda. exact=TRUE is load-bearing (dropping
    # it changes results materially); the model object carries its own alpha for
    # the exact refit, so only the unpacked scalar lambda is passed as s.

    pred <- glmnet::predict.glmnet(lasso_model, type="nonzero",
        s=lambda, exact=TRUE, newx=X, x=X, y=y)

    # predict.glmnet(type="nonzero") has never had a stable container. glmnet
    # 4.x returned a data.frame on a non-empty selection but a list on an empty
    # one; glmnet >= 5.0 always returns a list, one element per s value.
    # unlist() flattens every one of those shapes to the same integer vector, so
    # this is version-agnostic -- it is the idiom glmnet 5.0's own NEWS
    # prescribes for cross-version callers (#188).
    #
    # SAFE HERE ONLY BECAUSE s IS SCALAR, which two things together guarantee:
    # checkCssLassoInputs() rejects any lambda of length 3 or more, and the
    # unpacking above reduces the length-2 c(lambda=, alpha=) form to a single
    # penalty. (Both halves matter -- cssLasso() is exported, so it can be
    # called directly.) So pred has exactly one slot and unlist() cannot union
    # indices across s values.
    #
    # Do NOT copy this idiom to a call that leaves s unspecified: predict.glmnet
    # then returns one slot per penalty in the fitted path. clusterLassoCore()
    # does exactly that, and flattening its result would silently merge the
    # per-model-size sets that getClusterSelsFromGlmnet() consumes (#190).
    stopifnot(!("try-error" %in% class(pred) | "error" %in% class(pred) |
        "simpleError" %in% class(pred) | "condition" %in% class(pred)))

    selected_glmnet <- sort(unique(unlist(pred)))

    # No feature selected at this penalty: unlist() of the all-NULL result is
    # NULL, hence length 0. Replaces the previous is.null(pred[[1]]) test, which
    # inspected the container rather than the contents.
    if(length(selected_glmnet) == 0){
        return(integer())
    }

    stopifnot(length(selected_glmnet) <= ncol(X))
    stopifnot(all(selected_glmnet == round(selected_glmnet)))
    # No duplicate check is needed: nonzeroCoef() builds each slot as which[x],
    # a logical subset of distinct column indices, so duplicates cannot arise
    # upstream. (Asserting it after unique() would be a guardrail blinded by its
    # own input rather than a real check.)

    selected <- as.integer(selected_glmnet)

    return(selected)
}

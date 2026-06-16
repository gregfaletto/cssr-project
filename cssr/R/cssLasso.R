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

    # TODO(gregfaletto) allow cssLasso to accept a vector of lambda values rather
    # than just a single one.
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

    if(is.null(pred[[1]])){return(integer())}

    stopifnot(is.data.frame(pred))
    stopifnot(!("try-error" %in% class(pred) | "error" %in% class(pred) |
        "simpleError" %in% class(pred) | "condition" %in% class(pred)))

    if(length(dim(pred)) == 2){
        selected_glmnet <- pred[, 1]
    } else if(length(dim(pred)) == 3){
        selected_glmnet <- pred[, 1, 1]
    } else if(length(dim(pred)) == 1){
        selected_glmnet <- pred
    } else{
        stop("length(dim(pred)) not 1, 2, or 3")
    }

    stopifnot(length(selected_glmnet) >= 1)
    stopifnot(length(selected_glmnet) <= ncol(X))
    stopifnot(all(selected_glmnet == round(selected_glmnet)))
    stopifnot(length(selected_glmnet) == length(unique(selected_glmnet)))
    selected_glmnet <- as.integer(selected_glmnet)

    selected <- sort(selected_glmnet)

    return(selected)
}

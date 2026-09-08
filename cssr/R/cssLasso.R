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

    lasso_model <- glmnet::glmnet(x=X, y=y, family="gaussian", alpha=alpha)
    stopifnot(all.equal(class(lasso_model), c("elnet", "glmnet")))

    # Get coefficients at the desired lambda. predict.glmnet(exact=TRUE) used to
    # do this by refitting the whole path augmented with lambda -- including
    # every grid point BELOW lambda, which warm-started coordinate descent
    # solves after the target and which therefore cannot influence it. Instead
    # we fit that augmented grid ourselves, shortened by anchoredLambdaGrid()
    # and then predicted from with an ordinary exact=FALSE call. The shortened
    # grid keeps the augmented grid's own final element: predict.glmnet()
    # interpolates through glmnet:::lambda.interp(), which normalises by
    # lambda[1] - lambda[k], so the blend weight at lambda depends on the last
    # supplied grid point as well as the first, and dropping it changes the
    # answer in its last bits. anchoredLambdaGrid()'s n_interior argument is a
    # speed constant only; the coefficients here do not depend on it (#125).
    # The penalty finally predicted at is not lambda but snapLambdaToGrid()'s
    # reading of it off the fit; the comment at that call says why (#199).
    #
    # The short-circuit is a correctness requirement, not an optimisation.
    # Removing it changes the selected set on the on-grid route -- concentrated
    # at the top of the path, and by margins far larger than last-bit noise.
    # css() never reaches it (a subsample's target penalty is essentially never
    # one of that subsample's own fitted penalties); it is there to keep the
    # exported direct-call route byte-identical. The predicate is deliberately
    # the one predict.glmnet() itself applies, match(s, lambda, FALSE), so the
    # two agree about what "already on the grid" means.
    if(match(lambda, lasso_model$lambda, 0L) > 0){
        fit_at_lambda <- lasso_model
    } else{
        fit_at_lambda <- glmnet::glmnet(x=X, y=y, family="gaussian",
            alpha=alpha, lambda=anchoredLambdaGrid(lasso_model$lambda, lambda))
    }

    # Predict at the penalty this fit actually holds, not at the one we asked
    # for. glmnet reports a supplied grid back through the response scale --
    # divided by it and multiplied by it again -- so the entry that comes back
    # can sit an ULP away from what went in. predict.glmnet() with a numeric s
    # does not solve anything: it calls glmnet:::lambda.interp(), which locates
    # s between two grid columns and returns left, right and frac, and then
    # forms beta[, left]*frac + beta[, right]*(1 - frac). An ULP miss near the
    # top of the path leaves frac a hair short of 1, and every coefficient the
    # neighbouring column has and this one does not leaks in at the scale of
    # that hair -- around 1e-17, and SELECTED, because nonzeroCoef()'s
    # membership test is abs(x) > 0 with no tolerance. Handing s the fit's own
    # entry makes lambda.interp() report left == right with frac == 1, so what
    # comes back is the solved column with its exact zeros (#199).
    #
    # Applied on BOTH routes rather than only on the refit route. On the
    # short-circuit route match(lambda, lasso_model$lambda, 0L) > 0 has already
    # established an exact hit, so the helper returns lambda unchanged there and
    # test_that("cssLasso short-circuits when lambda is already on the path
    # (#125)") stays green with its assertions untouched -- which is how that
    # claim is confirmed, by running it rather than by asserting it here.
    s <- snapLambdaToGrid(fit_at_lambda$lambda, lambda)

    pred <- glmnet::predict.glmnet(fit_at_lambda, type="nonzero", s=s)

    # predict.glmnet(type="nonzero") has never had a stable container. glmnet
    # 4.x returned a data.frame whenever apply() could simplify -- i.e. whenever
    # the per-s index counts were uniform -- and a list otherwise; glmnet >= 5.0
    # always returns a list, one element per s value. For the scalar s used here
    # that rule makes 4.x yield a data.frame on a non-empty selection and a list
    # on an empty one, but the underlying rule is about simplification, not
    # emptiness. unlist() flattens every one of those shapes to the same integer
    # vector, so this is version-agnostic -- it is the idiom glmnet 5.0's own
    # NEWS prescribes for cross-version callers (#188).
    #
    # SAFE HERE ONLY BECAUSE s IS SCALAR, which two things together guarantee:
    # checkCssLassoInputs() rejects any lambda whose length is not 1 or 2, and
    # the unpacking above reduces the length-2 c(lambda=, alpha=) form to a
    # single penalty. (Both halves matter -- cssLasso() is exported, so it can
    # be called directly.) So pred has exactly one slot and unlist() cannot
    # union indices across s values.
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

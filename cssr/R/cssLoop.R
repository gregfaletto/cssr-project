# Generated from _main.Rmd: do not edit by hand

#' Helper function run on each subsample
#' 
#' Runs provided feature selection method `fitfun` on each subsample for cluster
#' stability selection (this function is called within `mclapply`).
#' @param input Could be one of two things:
#'   - `subsample`: An integer vector of size `n/2` containing the indices of the
#'   observations in the subsample.
#'   - `drop_var_input`: A named list containing two elements: one named
#'   "subsample" and the same as the previous description, and a logical vector
#'   named "feats_to_keep" containing the indices of the features to be
#'   automatically selected.
#'
#' (The first object is the output of the function `createSubsamples()` when the
#' provided `prop_feats_remove` is 0, the default, and the second object is the
#' output of `createSubsamples()` when `prop_feats_remove > 0`.)
#' @param x an n x p numeric matrix containing the predictors. (This should be
#' the full design matrix provided to css.)
#' @param y A response; can be any response that takes the form of a length n
#' vector and is used (or not used) by `fitfun`. Typically (and for default
#' `fitfun = cssLasso`), `y` should be an n-dimensional numeric vector containing the
#' response. This should be the full response provided to css.
#' @param lambda A tuning parameter or set of tuning parameters that may be used
#' by the feature selection method. For example, in the default case when
#' `fitfun = cssLasso`, `lambda` is a numeric: the penalty to use for each lasso
#' fit.
#' @param fitfun A function that takes in arguments X, y, and lambda and returns
#' a vector of indices of the columns of X (selected features).
#' @param seed Optional integer. If non-NULL, the random number generator is
#' seeded with `set.seed(seed)` immediately before `fitfun` is called, so that a
#' stochastic `fitfun` is reproducible and identical across serial and parallel
#' execution. Default NULL (no seeding). Supplied internally by `getSelMatrix`
#' (one reproducible seed per subsample); direct callers can leave it NULL.
#' @return An integer vector; the indices of the features selected by `fitfun`.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
cssLoop <- function(input, x, y, lambda, fitfun, seed=NULL){
    # Check inputs
    stopifnot(is.matrix(x))
    # NOTE: x is intentionally NOT re-scanned for NAs here. cssLoop is @noRd and
    # called only by getSelMatrix (via mclapply), which already validates that
    # the full x is NA-free once before the 2*B-subsample loop. Re-scanning the
    # whole n x p matrix every subsample was ~5s of pure redundant work at
    # n=p=2000 (#60); the upstream check makes it unnecessary on every reachable
    # path.

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

    # Seed immediately before fitfun so a stochastic fitfun is reproducible
    # across serial/parallel execution (issue #12). seed is NULL for the
    # deterministic default cssLasso path and for direct callers.
    if(!is.null(seed)){
        set.seed(seed)
    }

    selected <- do.call(fitfun, list(X=x[subsample, feats_to_keep],
        y=y[subsample], lambda=lambda))

    # Validate the RAW fitfun return against its documented contract (an
    # integer vector that is a subset of seq_len(p_sub), where p_sub is the
    # number of columns handed to fitfun) BEFORE remapping to original
    # feature indices. Remapping first -- which(feats_to_keep)[selected] --
    # would let R's index arithmetic silently turn contract violations
    # (negative, logical, fractional, or NULL returns) into
    # plausible-but-wrong selections (#151).
    p_sub <- sum(feats_to_keep)
    checkCssLoopOutput(selected, p_sub, seq_len(p_sub))

    selected <- which(feats_to_keep)[as.integer(selected)]

    # Defensive post-remap check: once the raw return validates and the
    # remap is a plain index lookup, the remapped indices are guaranteed a
    # valid subset of the original features -- but keep the check as cheap
    # insurance at the user-code boundary.
    checkCssLoopOutput(selected, p, as.integer(which(feats_to_keep)))

    return(as.integer(selected))
}

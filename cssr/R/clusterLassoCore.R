# Generated from _main.Rmd: do not edit by hand

#' Shared implementation of protolasso and clusterRepLasso
#'
#' Internal helper holding the common body of `protolasso()` and
#' `clusterRepLasso()`, which differ only in the `type` passed to `getXglmnet()`
#' (the sole point where the two procedures diverge: protolasso discards
#' non-prototype cluster members, clusterRepLasso replaces each cluster with its
#' representative).
#' @param X,y,clusters,nlambda As documented in `protolasso()` /
#' `clusterRepLasso()`.
#' @param type Character; either "protolasso" or "clusterRepLasso", passed to
#' `getXglmnet()` to select the design-matrix construction.
#' @return As documented in `protolasso()` / `clusterRepLasso()`.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
clusterLassoCore <- function(X, y, clusters, nlambda, type){

    # Handle and format inputs; get cluster prototypes
    ret <- processClusterLassoInputs(X, y, clusters, nlambda)

    x <- ret$x
    clusters <- ret$clusters
    prototypes <- ret$prototypes
    feat_names <- ret$var_names

    rm(ret)

    # Format the design matrix for glmnet according to the chosen procedure
    # (type = "protolasso" or "clusterRepLasso"); see getXglmnet().
    X_glmnet <- getXglmnet(x, clusters, type=type, prototypes=prototypes)

    # getXglmnet returns one column per cluster, so a single all-encompassing
    # cluster (or a genuine p < 2 input, which processClusterLassoInputs does not
    # rule out) yields a 1-column design that glmnet cannot fit ("x should be a
    # matrix with 2 or more columns"). Fail early with a message naming both
    # degenerate causes instead of surfacing glmnet's opaque error. This catches
    # both protolasso() and clusterRepLasso(), which route through here.
    if(ncol(X_glmnet) < 2){
        stop("protolasso()/clusterRepLasso() need at least 2 cluster representatives to fit the lasso, but the provided data yields only 1 (all features are in a single cluster, or p < 2).")
    }

    # Estimate the lasso on the cluster prototypes / representatives
    fit <- glmnet::glmnet(x=X_glmnet, y=y, family="gaussian", nlambda=nlambda)
    nonzero <- glmnet::predict.glmnet(fit, type="nonzero")

    # predict.glmnet(type = "nonzero") has no stable container across glmnet
    # versions, and both operations downstream are container-sensitive: unique()
    # dedupes list ELEMENTS but data.frame ROWS, and getClusterSelsFromGlmnet()'s
    # lengths() counts per-element lengths but data.frame ROWS PER COLUMN. So a
    # SINGLE-COLUMN data.frame here does not error--it returns a confidently
    # wrong set of model sizes (#190). (The multi-column layout happens to
    # degenerate to the right answer, because no two of its rows can be equal, so
    # unique() is a no-op on it. Normalise both anyway: the code should not depend
    # on which of the two arrives.)
    #
    # glmnet 5.0 always returns a list. glmnet 4.x returns one only because this
    # fit supplies no lambda: the auto-generated sequence starts at lambda_max,
    # where df == 0, so nonzeroCoef()'s nzel() yields NULL for that penalty,
    # apply() cannot simplify to a matrix, and 4.x's data.frame(which) coercion
    # is skipped. That is an accident of the sequence, not a contract, so
    # normalise rather than rely on it.
    #
    # The two data.frame layouts are NOT the same, which is why as.list() alone
    # would be wrong. When every penalty selected the same k >= 2 features,
    # apply() gives a k x n_pen matrix and data.frame() lays out one COLUMN per
    # penalty. When every penalty selected exactly ONE feature, apply() collapses
    # to a vector and data.frame() gives a single column with one ROW per
    # penalty--the convention inverts. n_pen tells them apart.
    #
    # Do NOT flatten with unlist(). cssLasso() does, correctly, because its
    # scalar s gives it exactly one slot; this result has one slot per model
    # size and flattening would merge them (#188). That function's comment
    # states the rule and already warns against copying its idiom here; this
    # one is the full account of nonzeroCoef()'s layouts. They are
    # complementary, not competing.
    #
    # n_pen is ncol(fit$beta) only because this call passes no s. With s
    # supplied, predict.glmnet interpolates to length(s) columns instead and this
    # derivation breaks--change n_pen to length(s) if that ever happens.
    n_pen <- ncol(fit$beta)

    if(is.data.frame(nonzero)){
        nonzero_mat <- as.matrix(nonzero)
        if(ncol(nonzero_mat) == n_pen){
            nonzero <- lapply(seq_len(n_pen), function(j){
                as.integer(nonzero_mat[, j])
            })
        } else{
            stopifnot(nrow(nonzero_mat) == n_pen)
            nonzero <- lapply(seq_len(n_pen), function(i){
                as.integer(nonzero_mat[i, ])
            })
        }
    }

    # One slot per penalty in the fitted path, on every glmnet version. Any
    # other container--nonzeroCoef()'s nr == 1 branch, which the
    # ncol(X_glmnet) < 2 guard above makes unreachable here--stops rather than
    # being silently misread downstream.
    stopifnot(is.list(nonzero))
    stopifnot(!is.data.frame(nonzero))
    stopifnot(identical(length(nonzero), n_pen))

    lasso_sets <- unique(nonzero)

    # Obtain a tidy list of selected sets--one for each model size
    cluster_sel_results <- getClusterSelsFromGlmnet(lasso_sets, clusters,
        prototypes, feat_names)

    return(list(selected_sets=cluster_sel_results$selected_sets,
        selected_clusts_list=cluster_sel_results$selected_clusts_list,
        beta=fit$beta))
}

# Generated from _main.Rmd: do not edit by hand

#' Estimate prototypes from a single cluster
#'
#' Takes in a single cluster, x, and y and returns an integer of the index of
#' the feature prototype (the feature from the cluster most correlated with the
#' response).
#'
#' @param cluster_members_i An integer vector of indices of features that are in
#' a common cluster. Must have length at least 2.
#' @param x n x p numeric matrix; design matrix.
#' @param y Numeric response vector. Note: in general, the css function does not
#' require y to be a numeric vector, because the provided fitfun could use a
#' different form of y (for example, a categorical response variable). However,
#' y must be numeric in order to provide prototypes because the prototypes are
#' determined using the correlation between cluster members (columns of x) and
#' y.
#' @return integer; the index of the feature identified as the prototype for
#' the cluster.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
identifyPrototype <- function(cluster_members_i, x, y){
    # Check input
    stopifnot(is.integer(cluster_members_i))
    # If cluster only has one member, that member is the prototype
    if(length(cluster_members_i) == 1){
        return(cluster_members_i)
    }

    # Choose which cluster member to represent cluster for stability
    # metric purposes by choosing the one most highly correlated
    # with y

    # All cluster-member correlations in one cor() call (corFunction did one
    # per member, re-standardizing y each time). Replicate corFunction's rules:
    # a constant column (or constant y) has undefined correlation -> 0. A
    # vectorized cor() can differ from the per-column path in the last ULP and
    # so flip which.max on a near-tie -> a different (equally-valid) prototype.
    if(length(unique(y)) == 1){
        warning("The second argument to corFunction only had one unique entry")
    }
    # suppressWarnings: base cor() emits "the standard deviation is zero" for a
    # constant column; corFunction returned 0 silently, so muffle ONLY the
    # cor() call (the explicit constant-y warning above is kept).
    cors_i <- suppressWarnings(abs(stats::cor(x[, cluster_members_i, drop = FALSE], y)))
    cors_i <- as.numeric(cors_i)
    cors_i[is.na(cors_i)] <- 0
    max_index_i <- which.max(cors_i)[1]

    stopifnot(length(max_index_i) == 1)
    stopifnot(max_index_i %in% 1:length(cluster_members_i))

    ret <- cluster_members_i[max_index_i]

    # Check output

    stopifnot(is.integer(ret))
    stopifnot(length(ret) == 1)
    stopifnot(ret %in% cluster_members_i)
    stopifnot(identical(x[, cluster_members_i][, max_index_i],
        x[, ret]))

    return(ret)
}

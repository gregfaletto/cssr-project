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

    cors_i <- apply(x[, cluster_members_i], 2, corFunction, y=y)
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

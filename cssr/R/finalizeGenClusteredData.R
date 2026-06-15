# Generated from _main.Rmd: do not edit by hand

#' Assemble and validate the output of a genClusteredData* generator
#'
#' Shared tail of `genClusteredData()`, `genClusteredDataWeighted()`, and
#' `genClusteredDataWeightedRandom()`: column-bind the freshly generated cluster
#' proxies with the remaining features, coerce the latent features to a matrix,
#' run the common output dimension checks, and return the standard
#' `list(X, y, Z, mu)`.
#' @param proxy_mat Numeric matrix; the n by n_clusters*cluster_size block of
#' generated cluster proxies.
#' @param other_X Numeric matrix; the weak-signal and noise features (the
#' non-proxy columns of X), as returned by `genZmuY()`.
#' @param y Numeric vector; the response, as returned by `genZmuY()`.
#' @param mu Numeric vector; the expected response, as returned by `genZmuY()`.
#' @param Z The latent features, as returned by `genZmuY()` (a numeric vector
#' when n_clusters == 1); coerced to a matrix here.
#' @param n Integer or numeric; the number of observations.
#' @param p Integer or numeric; the total number of features (columns of X).
#' @param n_clusters Integer or numeric; the number of latent variables.
#' @return A list with elements X (numeric matrix), y (numeric vector), Z
#' (numeric matrix), and mu (numeric vector).
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
finalizeGenClusteredData <- function(proxy_mat, other_X, y, mu, Z, n, p,
    n_clusters){
    X <- cbind(proxy_mat, other_X)
    Z <- as.matrix(Z)

    # Check output
    stopifnot(length(mu) == n)

    stopifnot(nrow(X) == n)
    stopifnot(ncol(X) == p)

    if(any(!is.na(Z))){
        stopifnot(nrow(Z) == n)
        stopifnot(ncol(Z) == n_clusters)
    }

    return(list(X=X, y=y, Z=Z, mu=mu))
}

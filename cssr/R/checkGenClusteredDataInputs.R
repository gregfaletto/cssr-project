# Generated from _main.Rmd: do not edit by hand

#' Check inputs to genClusteredData
#'
#' @param p Integer or numeric; the number of features to generate. The
#' generated X will have p columns.
#' @param k_unclustered Integer or numeric; the number of features in X that
#' will have nonzero coefficients in the true model for y among those features 
#' not generated from the n_clusters latent variables (called "weak signal" 
#' features in the simulations from Faletto and Bien 2022). The coefficients on
#' these features will be determined by beta_unclustered.
#' @param cluster_size Integer or numeric; for each of the n_clusters latent
#' variables, X will contain cluster_size noisy proxies that are correlated with
#' the latent variable.
#' @param n_clusters Integer or numeric; the number of latent variables to
#' generate, each of which will be associated with an observed cluster in X.
#' Must be at least 1. Default is 1.
#' @param sig_clusters Integer or numeric; the number of generated latent
#' features that will have nonzero coefficients in the true model for y (all of
#' them will have coefficient beta_latent). Must be less than or equal to
#' n_clusters. Default is 1.
#' @param rho Integer or numeric; the covariance of the proxies in each cluster
#' with the latent variable (and each other). Note that the correlation between
#' the features in the cluster will be rho/var. Can't equal 0. Default is 0.9.
#' @param beta_latent Integer or numeric; the coefficient used for all
#' sig_clusters latent variables that have nonzero coefficients in the true
#' model for y. Can't equal 0. Default is 1.5.
#' @param beta_unclustered Integer or numeric; the maximum coefficient in the
#' model for y among the k_unclustered features in X not generated from the
#' latent variables. The coefficients of the features will be
#' beta_unclustered/sqrt(1:k_unclustered). Can't equal 0. Default is 1.
#' @param snr Integer or numeric; the signal-to-noise ratio of the response
#' y. If sigma_eps_sq is not specified, the variance of the noise in y will be
#' calculated using the formula sigma_eps_sq = sum(mu^2)/(n * snr). Only one of
#' snr and sigma_eps_sq must be specified. Default is NA.
#' @param sigma_eps_sq Integer or numeric; the variance on the noise added
#' to y. Only one of snr and sigma_eps_sq must be specified. Default is NA.
#' @return A list of the following elements. \item{X}{An n x p numeric matrix of
#' n observations from a p-dimensional multivariate normal distribution
#' generated using the specified parameters. The first n_clusters times
#' cluster_size features will be the clusters of features correlated with the
#' n_clusters latent variables. The next k_unclustered features will be the
#' "weak signal" features, and the remaining p - n_clusters*cluster_size -
#' k_unclustered features will be the unclustered noise features.} \item{y}{A
#' length n numeric vector; the response generated from X, the latent features
#' from Z, and the coefficient vector, along with additive noise.} \item{Z}{The
#' latent features; either a numeric vector (if n_clusters > 1) or a numeric
#' matrix (if n_clusters > 1). Note that (X, Z) is multivariate Gaussian.}
#' item{mu}{A length `n` numeric vector; the expected response given X, Z, and
#' the true coefficient vector (equal to y minus the added noise).}
#' @author Gregory Faletto, Jacob Bien
checkGenClusteredDataInputs <- function(p, k_unclustered, cluster_size,
    n_clusters, sig_clusters, rho, beta_latent, beta_unclustered, snr,
    sigma_eps_sq){

    stopifnot(is.numeric(sig_clusters) | is.integer(sig_clusters))
    stopifnot(sig_clusters <= n_clusters)
    stopifnot(sig_clusters >= 0)
    stopifnot(sig_clusters == round(sig_clusters))
    
    stopifnot(is.numeric(n_clusters) | is.integer(n_clusters))
    stopifnot(n_clusters == round(n_clusters))
    # TODO(gregfaletto): is it easy to remove the requirement that n_clusters is
    # at least 1 (so that it's possible to generate data with no latent 
    # features)? If so, should only check that cluster_size >= 1 if n_clusters
    # >= 1, and in makeCovarianceMatrix function only need block_size >= 1
    # rather than 2.
    stopifnot(n_clusters >= 1)

    stopifnot(cluster_size >= 2)

    stopifnot(rho > 0)

    stopifnot(beta_latent != 0)
    stopifnot(beta_unclustered != 0)

    stopifnot(is.numeric(k_unclustered) | is.integer(k_unclustered))
    stopifnot(k_unclustered >= 2)
    stopifnot(k_unclustered == round(k_unclustered))

    stopifnot(p >= n_clusters*cluster_size + k_unclustered)

    # Same as make_sparse_blocked_linear_model_random, but ith coefficient
    # of weak signal features is beta_unclustered/sqrt(i) in order to have
    # a definitive ranking of weak signal features.
    if(is.na(snr) & is.na(sigma_eps_sq)){
        stop("Must specify one of snr or sigma_eps_sq")
    }

    if(is.na(snr)){
        stopifnot(all(!is.na(sigma_eps_sq)))
        stopifnot(is.numeric(sigma_eps_sq) | is.integer(sigma_eps_sq))
        stopifnot(length(sigma_eps_sq) == 1)
        stopifnot(sigma_eps_sq >= 0)
    } else{
        stopifnot(is.numeric(snr) | is.integer(snr))
        stopifnot(length(snr) == 1)
        stopifnot(snr > 0)
    }
}

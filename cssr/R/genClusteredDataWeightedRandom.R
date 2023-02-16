# Generated from _main.Rmd: do not edit by hand

#' Generate randomly sampled data including noisy observations of latent
#' variables, where proxies differ in their relevance (noise level)
#'
#' Generate a data set including latent features Z, observed features X (which
#' may include noisy or noiseless observations of the latent features in Z),
#' an observed response y which is a linear model of features from Z and X as
#' well as independent mean zero noise, and mu (the responses from y without
#' the added noise).
#' @param n Integer or numeric; the number of observations to generate. (The
#' generated X and Z will have n rows, and the generated y and mu will have
#' length n.)
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
#' @param rho_high Integer or numeric; the maximum correlation of the proxies
#' each cluster with each other. Default is 1.
#' @param rho_low Integer or numeric; the minimum correlation of the proxies in
#' each cluster with each other. rho_low cannot equal 0 and must be no larger
#' than rho_high. Default is 0.5.
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
#' \item{mu}{A length `n` numeric vector; the expected response given X, Z, and
#' the true coefficient vector (equal to y minus the added noise).}
#' @author Gregory Faletto, Jacob Bien
#' @references
#' Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
#' @export
genClusteredDataWeightedRandom <- function(n, p, k_unclustered, cluster_size,
    n_clusters=1, sig_clusters=1, rho_high=1, rho_low=0.5, beta_latent=1.5,
    beta_unclustered=1, snr=as.numeric(NA), sigma_eps_sq=as.numeric(NA)){

    # Check inputs
    checkGenClusteredDataWeightedRandomInputs(p, k_unclustered, cluster_size,
        n_clusters, sig_clusters, rho_high, rho_low,
        beta_latent, beta_unclustered, snr, sigma_eps_sq)

    ret <- genZmuY(n=n, p=p, k_unclustered=k_unclustered,
        cluster_size=cluster_size, n_clusters=n_clusters,
        sig_clusters=sig_clusters, beta_latent=beta_latent,
        beta_unclustered=beta_unclustered, snr=snr, sigma_eps_sq=sigma_eps_sq)

    Z <- ret$Z
    y <- ret$y
    mu <- ret$mu
    other_X <- ret$other_X

    # Finally, generate clusters of proxies to complete X.
    # Create matrix of proxies
    Z <- as.matrix(Z)
    proxy_mat <- matrix(as.numeric(NA), n, n_clusters*cluster_size)
    for(i in 1:n_clusters){
        for(j in 1:cluster_size){
            # Choose correlation at random
            rho_ij <- runif(n=1, min=rho_low, max=rho_high)
            # Get noise variance
            noise_var_ij <- getNoiseVar(rho_ij)
            proxy_mat[, (i - 1)*cluster_size + j] <- Z[, i] + rnorm(n,
                mean=0, sd=sqrt(noise_var_ij))
        }
    }

    X <- cbind(proxy_mat, other_X)
    
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

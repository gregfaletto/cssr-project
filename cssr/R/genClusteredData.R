# Generated from create-cssr.Rmd: do not edit by hand

#' Generate randomly sampled data including noisy observations of latent
#' variables
#'
#' TODO(gregfaletto) change cluster_size into a vector of sizes (maybe also
#' deprecate n_clusters as an input, since this would be inferred by the length
#' of cluster_sizes?)
#' Generate a data set including latent features Z, observed features X (which
#' may include noisy or noiseless observations of the latent features in Z),
#' an observed response y which is a linear model of features from Z and X as
#' well as independent mean zero noise, and mu (the responses from y without
#' the added noise). Data is generated in the same way as in the simulations
#' from Faletto and Bien (2022).
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
#' @param rho Integer or numeric; the covariance of the proxies in each cluster
#' with the latent variable (and each other). Note that the correlation between
#' the features in the cluster will be rho/var. Can't equal 0. Default is 0.9.
#' @param var Integer or numeric; the variance of all of the observed features
#' in X (both the proxies for the latent variables and the k_unclustered other
#' features). Can't equal 0. Default is 1.
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
#' @references Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
#' @export
genClusteredData <- function(n, p, k_unclustered, cluster_size, n_clusters=1,
    sig_clusters=1, rho=0.9, var=1, beta_latent=1.5, beta_unclustered=1,
    snr=as.numeric(NA), sigma_eps_sq=as.numeric(NA)){

    # Check inputs
    checkGenClusteredDataInputs(p, k_unclustered, cluster_size, n_clusters,
        sig_clusters, rho, var, beta_latent, beta_unclustered, snr,
        sigma_eps_sq)

    # Generate covariance matrix (latent features are mixed in matrix, so each
    # cluster will be of size cluster_size + 1)
    Sigma <- makeCovarianceMatrix(p=p + n_clusters, nblocks=n_clusters,
        block_size=cluster_size + 1, rho=rho, var=var)

    # Generate coefficients
    # Note that beta has length p + sig_clusters
    coefs <- makeCoefficients(p=p + n_clusters, k_unblocked=k_unclustered,
        beta_low=beta_unclustered, beta_high=beta_latent, nblocks=n_clusters,
        sig_blocks=sig_clusters, block_size=cluster_size + 1)

    # Generate mu, X, z, sd, y
    gen_mu_x_z_sd_res <- genMuXZSd(n=n, p=p, beta=coefs$beta, Sigma=Sigma,
        blocked_dgp_vars=coefs$blocked_dgp_vars, latent_vars=coefs$latent_vars, 
        block_size=cluster_size, n_blocks=n_clusters, snr=snr,
        sigma_eps_sq=sigma_eps_sq)

    mu <- gen_mu_x_z_sd_res$mu
    sd <- gen_mu_x_z_sd_res$sd

    y <- mu + sd * stats::rnorm(n)

    return(list(X=gen_mu_x_z_sd_res$X, y=y, Z=gen_mu_x_z_sd_res$z, mu=mu))
}

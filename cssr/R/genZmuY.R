# Generated from _main.Rmd: do not edit by hand

#' Generates Z, weak signal features in X, noise features in X, mu, and y
#' from provided parameters
#'
#' @param n Integer or numeric; the number of observations to generate. (The
#' generated X and Z will have n rows, and the generated y and mu will have
#' length n.)
#' @param p Integer or numeric; the number of features to generate. The
#' generated X will have p columns.
#' @param k_unclustered Integer or numeric; the number of features in X that
#' will have nonzero coefficients in the true model for y among those features 
#' not generated from the n_clusters latent variables (called "weak signal" 
#' features in the simulations from Faletto and Bien 2022). The coefficients on
#' these features will be determined by beta_unclustered. Must be at least 1.
#' @param cluster_size Integer or numeric; for each of the n_clusters latent
#' variables, X will contain cluster_size noisy proxies that are correlated with
#' the latent variable. Must be at least 2.
#' @param n_clusters Integer or numeric; the number of latent variables to
#' generate, each of which will be associated with an observed cluster in X.
#' Must be at least 1. Default is 1.
#' @param sig_clusters Integer or numeric; the number of generated latent
#' features that will have nonzero coefficients in the true model for y (all of
#' them will have coefficient beta_latent). Must be less than or equal to
#' n_clusters. Default is 1.
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
#' @return A list of the following elements. \item{Z}{The
#' latent features; either a numeric vector (if n_clusters > 1) or a numeric
#' matrix (if n_clusters > 1). Note that (X, Z) is multivariate Gaussian.}
#' item{mu}{A length `n` numeric vector; the expected response given X, Z, and
#' the true coefficient vector (equal to y minus the added noise).} \item{y}{A
#' length n numeric vector; the response generated from X, the latent features
#' from Z, and the coefficient vector, along with additive noise.} 
#' \item{other_X}{A numeric matrix of n observations from a multivariate normal
#' distribution generated using the specified parameters, containing the weak
#' signal features and the noise features that will eventually be in X. (The
#' only missing features are the proxies for the latent features Z.)
#' @author Gregory Faletto, Jacob Bien
#' @references
#' Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
genZmuY <- function(n, p, k_unclustered, cluster_size, n_clusters, sig_clusters,
    beta_latent, beta_unclustered, snr, sigma_eps_sq){
    # Generate Z, weak signal features, and noise features (total of
    # p - n_clusters*(cluster_size - 1)) features)
    p_orig_feat_mat <- p - n_clusters*(cluster_size - 1)
    stopifnot(p_orig_feat_mat >= k_unclustered + n_clusters)
    orig_feat_mat <- matrix(stats::rnorm(n*p_orig_feat_mat), n, p_orig_feat_mat)
    # First n_clusters features are Z. Next k_unclustered features are weak
    # signal features. Any remaining features are noise features.
    Z <- orig_feat_mat[, 1:n_clusters]

    other_X <- orig_feat_mat[, (n_clusters + 1):p_orig_feat_mat]

    # Ready to create mu and y
    if(n_clusters > 1){
        if(sig_clusters > 1){
            mu <- Z[, 1:sig_clusters] %*% rep(beta_latent, sig_clusters) +
                other_X[, 1:k_unclustered] %*% rep(beta_unclustered,
                    k_unclustered)
        } else{
            mu <- Z[, 1:sig_clusters] * beta_latent +
                other_X[, 1:k_unclustered] %*% rep(beta_unclustered,
                    k_unclustered)
        }
        
    } else{
        mu <- Z*beta_latent + other_X[, 1:k_unclustered] %*%
            rep(beta_unclustered, k_unclustered)
    }
    mu <- as.numeric(mu)

    # If SNR is null, use sigma_eps_sq
    if(!is.na(sigma_eps_sq)){
        sd <- sqrt(sigma_eps_sq)
    }else{
        sd <- sqrt(sum(mu^2) / (n * snr)) # taking snr = ||mu||^2 /(n * sigma^2)
    }

    stopifnot(is.numeric(sd) | is.integer(sd))
    stopifnot(length(sd) == 1)
    stopifnot(!is.na(sd))
    stopifnot(sd >= 0)

    y <- as.numeric(mu + rnorm(n, mean=0, sd=sd))

    return(list(Z=Z, mu=mu, y=y, other_X=other_X))
}

# Generated from create-cssr.Rmd: do not edit by hand

#' Generate observed and latent variables along with conditional mean
#'
#' @param n Integer or numeric; the number of observations to generate. (The
#' generated X and Z will have n rows, and the generated y and mu will have
#' length n.)
#' @param p Integer or numeric; the number of observed features (the generated X
#' will have p columns).
#' @param beta A numeric or integer vector of length `p` + sig_blocks containing
#' the coefficients for the true model for y.
#' @param Sigma A (`p` + n_blocks) x (`p` + n_blocks) numeric matrix
#' representing the covariance matrix for the latent features, the associated
#' proxies, and the remaining features.
#' @param blocked_dgp_vars An integer vector of length sig_blocks containing the
#' indices of the features corresponding to the latent features that have
#' nonzero coefficient beta_high in the true model for y.
#' @param latent_vars An integer vector of length n_blocks containing the
#' indices of all of the latent features.
#' @param n_blocks Integer or numeric; the number of latent variables to
#' generate, each of which will be associated with an observed cluster in X.
#' Must be at least 1. Default is 1.
#' @param block_size Integer or numeric; for each of the n_blocks latent
#' variables, X will contain block_size noisy proxies that are correlated with
#' the latent variable.
#' @param snr Integer or numeric; the signal-to-noise ratio of the response
#' y. If sigma_eps_sq is not specified, the variance of the noise in y will be
#' calculated using the formula sigma_eps_sq = sum(mu^2)/(n * snr). Only one of
#' snr and sigma_eps_sq must be specified. Default is NA.
#' @param sigma_eps_sq Integer or numeric; the variance on the noise added
#' to y. Only one of snr and sigma_eps_sq must be specified. Default is NA.
#' @return A named list with the following elements: \item{X}{An `n` x `p`
#' numeric matrix containing the observed proxies for the latent variables as
#' well as the observed unblocked (iid) variables.} \item{mu}{A length `n`
#' numeric vector; the expected response given X, Z, and the true
#' coefficient vector (equal to y minus the added noise).} \item{z}{An `n` x
#' n_blocks numeric matrix containing the n_blocks latent variables. Note that
#' (X, z) is multivariate Gaussian.} \item{sd}{Numeric; the standard deviation
#' of the noise added to mu to get y (calculated either from snr or
#' sigma_eps_sq).}
#' @author Gregory Faletto, Jacob Bien
genMuXZSd <- function(n, p, beta, Sigma, blocked_dgp_vars,
    latent_vars, n_blocks=1, block_size, snr=NA, sigma_eps_sq=NA){
    # Check inputs

    stopifnot(length(blocked_dgp_vars) <= n_blocks)
    stopifnot(nrow(Sigma) == p + n_blocks)
    stopifnot(ncol(Sigma) == p + n_blocks)
    
    if(any(!is.na(sigma_eps_sq))){
        stopifnot(is.numeric(sigma_eps_sq) | is.integer(sigma_eps_sq))
        stopifnot(length(sigma_eps_sq) == 1)
        stopifnot(sigma_eps_sq >= 0)
    } else{
        if(any(is.na(snr))){
            stop("Must provide one of snr or sigma_eps_sq")
        }
        stopifnot(is.numeric(snr) | is.integer(snr))
        stopifnot(length(snr) == 1)
        stopifnot(snr > 0)
    }

    stopifnot(length(beta) == p + n_blocks)
    stopifnot(all(beta[blocked_dgp_vars] != 0))

    stopifnot(length(latent_vars) == n_blocks)

    x <- MASS::mvrnorm(n=n, mu=rep(0, p + n_blocks), Sigma=Sigma)

    stopifnot(length(beta) == ncol(x))

    mu <- as.numeric(x %*% beta)

    # Remove true blocked signal feature from each block from x now that I've
    # generated mu
    if(n_blocks > 0){
        z <- matrix(as.numeric(NA), nrow=n, ncol=n_blocks)
        stopifnot(length(latent_vars) > 0)
    } else{
        z <- NA
        stopifnot(length(latent_vars) == 0)
    }
    
    if(length(latent_vars) > 0){
        if(length(latent_vars) > 0){
        z[, 1:n_blocks] <- x[, latent_vars]
    }
    }
    
    x <- x[, setdiff(1:(p + n_blocks), latent_vars)]

    # If SNR is null, use sigma_eps_sq
    if(!is.na(sigma_eps_sq)){
        sd <- sqrt(sigma_eps_sq)
    }else{
        sd <- sqrt(sum(mu^2) / (n * snr)) # taking snr = ||mu||^2 /(n * sigma^2)
    }

    # Check output

    stopifnot(length(mu) == n)

    stopifnot(nrow(x) == n)
    stopifnot(ncol(x) == p)

    if(any(!is.na(z))){
        stopifnot(nrow(z) == n)
        stopifnot(ncol(z) == n_blocks)
    }

    stopifnot(is.numeric(sd) | is.integer(sd))
    stopifnot(length(sd) == 1)
    stopifnot(!is.na(sd))
    stopifnot(sd >= 0)

    return(list(X=x, mu=mu, z=z, sd=sd))
}

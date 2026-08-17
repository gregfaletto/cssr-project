# Generated from _main.Rmd: do not edit by hand

#' Get variance of noise to add to Z in order to yield proxies X with desired
#' correlations with Z
#'
#' @param rho A numeric vector of desired correlations for each proxy to have
#' with Z. Note: correlations must be in (0, 1] (positive and at most 1).
#' @return A vector of variances of independent Gaussian random variables to add
#' to Z in order to yield proxies with the desired correlations with Z.
#' @author Gregory Faletto, Jacob Bien
#' @examples
#' # Noise variance needed so a proxy Z + N(0, v) attains each target
#' # correlation with Z (correlations must be in (0, 1] (positive and at most 1)).
#' getNoiseVar(c(0.9, 0.5, 1))
#' @export
getNoiseVar <- function(rho){
    # Correlation between standard normal Z and X = Z + epsilon where epsilon
    # is normal, independent of Z, and has mean 0 and variance sig_eps_sq:
    # 
    #   E[Z X]/sqrt{Var(Z) Var(X)}
    # = (E[Z^2] + E[Z*epsilon])/sqrt{1*(1 + sig_eps_sq)}
    # = (1 + 0)/sqrt{1 + sig_eps_sq}
    #
    # So we have
    #                 rho = 1/sqrt{1 + sig_eps_sq}
    # \iff 1 + sig_eps_sq = 1/rho^2
    # \iff     sig_eps_sq = 1/rho^2 - 1
    stopifnot(is.numeric(rho) | is.integer(rho))
    stopifnot(all(!is.na(rho)))
    stopifnot(length(rho) >= 1)
    stopifnot(all(rho > 0))
    stopifnot(all(rho <= 1))
    return(1/rho^2 - 1)
}


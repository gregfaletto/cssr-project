# Generated from _main.Rmd: do not edit by hand

#' Get variance of noise to add to Z in order to yield proxies X with desired
#' correlations
#'
#' @param cor A numeric vector of desired correlations for each proxy to have
#' with Z. Note: correlations must be positive.
#' @return A vector of variances of independent Gaussian random variables to add
#' to Z in order to yield proxies with the desired correlations with Z.
#' @author Gregory Faletto, Jacob Bien
getNoiseVar <- function(cor){
    # Correlation between standard normal Z and X = Z + epsilon where epsilon
    # is normal, independent of Z, and has mean 0 and variance sig_eps_sq:
    # 
    #   E[Z X]/sqrt{Var(Z) Var(X)}
    # = (E[Z^2] + E[Z*epsilon])/sqrt{1*(1 + sig_eps_sq)}
    # = (1 + 0)/sqrt{1 + sig_eps_sq}
    #
    # So we have
    #                 cor = 1/sqrt{1 + sig_eps_sq}
    # \iff 1 + sig_eps_sq = 1/cor^2
    # \iff     sig_eps_sq = 1/cor^2 - 1
    stopifnot(is.numeric(cor) | is.integer(cor))
    stopifnot(all(!is.na(cor)))
    stopifnot(length(cor) >= 1)
    stopifnot(all(cor > 0))
    stopifnot(all(cor <= 1))
    return(1/cor^2 - 1)
}


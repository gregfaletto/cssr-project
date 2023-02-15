# Generated from _main.Rmd: do not edit by hand

#' Get variance of noise to add to Z in order to yield proxies X with desired
#' correlations between the proxies
#'
#' @param cor A numeric vector of desired correlations for each proxy to have
#' with the other proxies in the cluster. Note: correlations must be positive.
#' @return A vector of variances of independent Gaussian random variables to add
#' to Z in order to yield proxies with the desired correlations with Z.
#' @author Gregory Faletto, Jacob Bien
getNoiseVar <- function(cor){
    # Correlation between X_1 = Z + epsilon_1 and X_2 = Z + epsilon_2 where
    # Z is standard normal and epsilon_i are both normal, independent of Z,
    # and have mean 0 and variance sig_eps_sq:
    # 
    #   E[X_1 X_2]/sqrt{Var(X_1) Var(X_2)}
    # = (E[Z^2] + E[Z*epsilon_1] + E[Z*epsilon_2] + E[epsilon_1*epsilon_2])/
    #   sqrt{(1 + sig_eps_sq)^2}
    # = (1 + 0 + 0 + 0)/(1 + sig_eps_sq)
    #
    # So we have
    #             cor = 1/(1 + sig_eps_sq)
    # \iff sig_eps_sq = 1/cor - 1
    stopifnot(is.numeric(cor) | is.integer(cor))
    stopifnot(all(!is.na(cor)))
    stopifnot(length(cor) >= 1)
    stopifnot(all(cor > 0))
    stopifnot(all(cor <= 1))
    return(1/cor - 1)
}


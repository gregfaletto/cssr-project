# Generated from _main.Rmd: do not edit by hand

#' Shared leading input checks for the genClusteredData* generators
#'
#' The checks every genClusteredData* validator runs before its
#' correlation-specific checks (sig_clusters, n_clusters, and cluster_size).
#' @param sig_clusters,n_clusters,cluster_size As documented in
#' `checkGenClusteredDataInputs()`.
#' @return No return value; called for the side effect of erroring on bad input.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
checkGenClusteredDataInputsPre <- function(sig_clusters, n_clusters,
    cluster_size){

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
}

#' Shared trailing input checks for the genClusteredData* generators
#'
#' The checks every genClusteredData* validator runs after its
#' correlation-specific checks: the beta coefficients, k_unclustered, the
#' feature-count lower bound, and that exactly one of snr / sigma_eps_sq is
#' given. (Each validator then runs its own type/range checks on whichever of
#' snr / sigma_eps_sq was supplied.)
#' @param p,k_unclustered,cluster_size,n_clusters,beta_latent,beta_unclustered,snr,sigma_eps_sq
#' As documented in `checkGenClusteredDataInputs()`.
#' @return No return value; called for the side effect of erroring on bad input.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
checkGenClusteredDataInputsPost <- function(p, k_unclustered, cluster_size,
    n_clusters, beta_latent, beta_unclustered, snr, sigma_eps_sq){

    stopifnot(beta_latent != 0)
    stopifnot(beta_unclustered != 0)

    stopifnot(is.numeric(k_unclustered) | is.integer(k_unclustered))
    stopifnot(k_unclustered >= 1)
    stopifnot(k_unclustered == round(k_unclustered))

    stopifnot(p >= n_clusters*cluster_size + k_unclustered)

    # Same as make_sparse_blocked_linear_model_random, but ith coefficient
    # of weak signal features is beta_unclustered/sqrt(i) in order to have
    # a definitive ranking of weak signal features.
    if(is.na(snr) & is.na(sigma_eps_sq)){
        stop("Must specify one of snr or sigma_eps_sq")
    }

    if(!is.na(snr) & !is.na(sigma_eps_sq)){
        stop("Only one of snr and sigma_eps_sq may be specified")
    }
}

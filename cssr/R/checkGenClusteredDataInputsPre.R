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
    stopifnot(n_clusters >= 1)

    stopifnot(is.numeric(cluster_size) | is.integer(cluster_size))
    stopifnot(cluster_size == round(cluster_size))
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

    stopifnot(is.numeric(p) | is.integer(p))
    stopifnot(p == round(p))
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

#' Shared snr / sigma_eps_sq input checks for the genClusteredData* generators
#'
#' Validates whichever of snr / sigma_eps_sq was supplied (exactly one is non-NA
#' by this point, enforced by `checkGenClusteredDataInputsPost()`): it must be
#' numeric and length 1, and positive (snr) or non-negative (sigma_eps_sq).
#' Previously only `genClusteredData()` ran the full type/length checks; the two
#' weighted generators ran a leaner version that silently accepted a
#' non-numeric snr / sigma_eps_sq (which then failed later inside `genZmuY()`).
#' Sharing this helper makes the three consistent (#35).
#' @param snr,sigma_eps_sq As documented in `checkGenClusteredDataInputs()`.
#' @return No return value; called for the side effect of erroring on bad input.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
checkGenClusteredDataInputsSnrSigma <- function(snr, sigma_eps_sq){
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

#' Shared correlation-argument checks for the weighted genClusteredData*
#' generators
#'
#' Validates rho_high and rho_low for
#' `checkGenClusteredDataWeightedInputs()` and
#' `checkGenClusteredDataWeightedRandomInputs()`: each must be a single
#' non-missing number, with 0 < rho_low <= rho_high <= 1.
#'
#' Two things here are contract, not style. The formals must stay named
#' `rho_high` / `rho_low`, because `stopifnot()` deparses the expression as
#' written here, so the FORMAL's name is what the user sees (#162). And the
#' type/length/NA checks must precede every range check: `"0.9" >= "0.5"` is
#' TRUE, since R compares character to numeric as strings; `!is.na()` is
#' vectorized, so a length-2 argument containing NA reports the plural "are
#' not all TRUE" rather than the length violation; and
#' `stopifnot(numeric(0) <= 1)` does not error at all, since
#' `all(logical(0))` is TRUE. The tests pin every message below, so changing
#' either turns many expectations red at once -- the failure mode to guard
#' against is "repairing" them to match the new messages, which ships #162's
#' defect green.
#' @param rho_high,rho_low As documented in
#' `checkGenClusteredDataWeightedInputs()`.
#' @return No return value; called for the side effect of erroring on bad input.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
checkRhoHighLow <- function(rho_high, rho_low){
    stopifnot(is.numeric(rho_high) | is.integer(rho_high))
    stopifnot(length(rho_high) == 1)
    stopifnot(!is.na(rho_high))

    stopifnot(is.numeric(rho_low) | is.integer(rho_low))
    stopifnot(length(rho_low) == 1)
    stopifnot(!is.na(rho_low))

    stopifnot(rho_high <= 1)
    stopifnot(rho_high > 0)
    stopifnot(rho_low > 0)
    stopifnot(rho_high >= rho_low)
}

# Generated from create-cssr.Rmd: do not edit by hand

#' Generated coefficients for y in latent variable model
#'
#' @param p Integer or numeric; the number of features that will be observed in
#' x plus the number of latent variables (each corresponding to a cluster).
#' @param k_unblocked Integer or numeric; the number of features in X that
#' will have nonzero coefficients in the true model for y among those features 
#' not generated from the n_clusters latent variables (called "weak signal" 
#' features in the simulations from Faletto and Bien 2022). The coefficients on
#' these features will be determined by beta_low.
#' @param beta_low Integer or numeric; the maximum coefficient in the
#' model for y among the k_unblocked features in X not generated from the
#' latent variables. The coefficients of the features will be
#' beta_low/sqrt(1:k_unblocked).
#' @param beta_high Integer or numeric; the coefficient used for all
#' sig_blocks latent variables that have nonzero coefficients in the true
#' model for y.
#' @param nblocks Integer or numeric; the number of latent variables that were
#' generated, each of which will be associated with an observed cluster in X.
#' @param sig_blocks Integer or numeric; the number of generated latent
#' features that will have nonzero coefficients in the true model for y (all of
#' them will have coefficient beta_latent). In particular, the first sig_blocks
#' latent variables will have coefficient beta_latent, and the remaining nblocks
#' - sig_blocks features will have coefficient 0. Must be less than or equal to
#' n_clusters.
#' @param block_size Integer or numeric; for each of the n_blocks latent
#' variables, the covariance matrix will include the original latent feature
#' plus block_size - 1 noisy proxies that are correlated with the latent
#' variable.
#' @return A named list with the following elements: \item{beta}{A vector of
#' length `p` containing the coefficients for the true model for y. All entries
#' will equal 0 except for the sig_blocks latent variables that will have
#' coefficient beta_high and the k_unblocked independent features with
#' coefficient determined by beta_low.} \item{blocked_dgp_vars}{An integer
#' vector of length sig_blocks containing the indices of the features
#' corresponding to the latent features that will have nonzero coefficient
#' beta_high in the true model for y.} \item{sig_unblocked_vars}{An integer
#' vector of length k_unblocked containing the indices of the observed features
#' that are independent of the blocked features and have coefficient beta_low in
#' the true model for y. If k_unblocked = 0, this will just be NA.}
#' \item{insig_blocked_vars}{An integer vector containing the indices of the
#' features corresponding to the latent features that will have coefficient 0 in
#' the true model for y. If nblocks=0, this will just be NA.}
#' \item{latent_vars}{An integer vector of length nblocks containing the indices
#' of all of the latent features.}
#' @author Gregory Faletto, Jacob Bien
#' @references Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}.
makeCoefficients <- function(p, k_unblocked, beta_low, beta_high, nblocks,
    sig_blocks, block_size){

    # Check inputs
    stopifnot(k_unblocked >= 0)
    stopifnot(sig_blocks <= nblocks)
    stopifnot(p >= nblocks*block_size + k_unblocked)
    stopifnot(sig_blocks >= 0)

    # Initialize beta
    beta <- numeric(p)

    # identify indices of first coefficient in each significant block (these
    # features will have coefficient beta_high)
    latent_vars <- NA
    if(nblocks >= 1){
        latent_vars <- as.integer(((0:(nblocks - 1))*block_size + 1))

        stopifnot(all(latent_vars) %in% 1:p)
        stopifnot(all(latent_vars) %in% 1:(block_size*nblocks))
        stopifnot(length(unique(latent_vars)) == nblocks)
        stopifnot(length(latent_vars) == nblocks)
    }

    blocked_dgp_vars <- latent_vars[1:sig_blocks]

    stopifnot(sig_blocks == length(blocked_dgp_vars))
    
    beta[blocked_dgp_vars] <- beta_high

    # identify remaining coefficients in blocks (which ought to be set to 0)
    insig_blocked_vars <- NA

    if(nblocks >= 1){
        insig_blocked_vars <- setdiff(1:(block_size*nblocks), blocked_dgp_vars)
        stopifnot(all(beta[insig_blocked_vars] == 0))
    }
    # find significant unblocked variables (if applicable) and fill in
    # coefficients
    sig_unblocked_vars <- NA

    if(k_unblocked > 0){
        # Range of weak signal coefficients
        beta_lows <- beta_low/sqrt(1:k_unblocked)
        sig_unblocked_vars <- (nblocks*block_size + 1):
            (nblocks*block_size + k_unblocked)
        sig_unblocked_vars <- as.integer(sig_unblocked_vars)

        stopifnot(length(sig_unblocked_vars) == k_unblocked)
        stopifnot(length(unique(sig_unblocked_vars)) == k_unblocked)
        stopifnot(all(sig_unblocked_vars) %in% 1:p)

        beta[sig_unblocked_vars] <- beta_lows
    }

    stopifnot(length(intersect(blocked_dgp_vars, sig_unblocked_vars)) == 0)
    stopifnot(length(intersect(sig_unblocked_vars, insig_blocked_vars)) == 0)
    stopifnot(length(intersect(blocked_dgp_vars, insig_blocked_vars)) == 0)

    stopifnot(length(insig_blocked_vars) + length(blocked_dgp_vars) ==
        nblocks*block_size)

    stopifnot(sig_blocks + length(insig_blocked_vars) + k_unblocked <= p)

    stopifnot(sum(beta != 0) == sig_blocks + k_unblocked)
    stopifnot(is.numeric(beta) | is.integer(beta))

    return(list(beta=beta, blocked_dgp_vars=blocked_dgp_vars,
        sig_unblocked_vars=sig_unblocked_vars,
        insig_blocked_vars=insig_blocked_vars, latent_vars=latent_vars))
}

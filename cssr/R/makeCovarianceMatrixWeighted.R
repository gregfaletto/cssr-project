# Generated from _main.Rmd: do not edit by hand

#' Generate covariance matrix for simulated clustered data
#'
#' @param p Integer or numeric; the total number of features in the covariance
#' matrix to be created, including latent features, the associated noisy proxies
#' with each latent feature, and other (weak signal and noise) features.
#' @param n_blocks Integer or numeric; the number of latent variables in the
#' data, each of which is associated with an observed cluster in X. Must be at
#' least 1.
#' @param block_size Integer or numeric; for each of the n_blocks latent
#' variables, the covariance matrix will include the original latent feature
#' plus block_size - 1 noisy proxies that are correlated with the latent
#' variable.
#' @param n_strong_block_vars Integer or numeric; the number of proxies that
#' will have a high covariance (rho_high) with the corresponding latent 
#' features. (The remaining block_size - n_strong_block_vars cluster members
#' will have covariance rho_low with the latent feature.)
#' @param rho_high Integer or numeric; the covariance of the "strong proxies" in
#' each cluster with the latent variable (and each other). Note that the
#' correlation between the "strong proxy" features in the cluster will be
#' rho_high/var. rho_high cannot equal 0 and must be at least as large as
#' rho_low.
#' @param rho_low Integer or numeric; the covariance of the "weak proxies" in
#' each cluster with the latent variable (and each other). Note that the
#' correlation between the "weak proxy" features in the cluster will be
#' rho_low/var. rho_low cannot equal 0 and must be no larger than rho_high.
#' @param var Integer or numeric; the variance of all of the observed features
#' in X (both the proxies for the latent variables and the k_unclustered other
#' features). var cannot equal 0.
#' @return A `p` x `p` numeric matrix representing the covariance matrix for
#' the latent features, the associated proxies, and the remaining features. All
#' features not in a block will be independent from each other and the blocks
#' and have variance var.
#' @author Gregory Faletto, Jacob Bien
makeCovarianceMatrixWeighted <- function(p, nblocks, block_size,
    n_strong_block_vars, rho_high, rho_low, var) {
    # Check inputs

    stopifnot(nblocks >= 1)
    stopifnot(rho_high != 0)
    stopifnot(rho_low != 0)
    stopifnot(abs(rho_high) >= abs(rho_low))
    stopifnot(var != 0)
    stopifnot(abs(rho_high) <= abs(var))
    stopifnot(block_size >= 2)
    stopifnot(p >= nblocks*block_size)
    stopifnot(n_strong_block_vars <= block_size)

    # start with p x p identity matrix
    Sigma <- var*diag(p)

    # create matrix with nblocks rows, each containing a vector of
    # indices of highly correlated features
    block_feats <- matrix(seq(nblocks*block_size), nrow=nblocks, byrow=TRUE)

    stopifnot(length(unique(block_feats)) == length(block_feats))

    # add covariances of highly correlated features to sigma
    for(i in 1:nblocks){
        for(j in 1:(block_size - 1)){
            for(k in (j+1):block_size){
                feat_1 <- block_feats[i, j]
                feat_2 <- block_feats[i, k]
                Sigma[feat_1, feat_2] <- rho_low
                Sigma[feat_2, feat_1] <- rho_low
            }
        }

        for(j in 1:(n_strong_block_vars - 1)){
            for(k in (j+1):n_strong_block_vars){
                feat_1 <- block_feats[i, j]
                feat_2 <- block_feats[i, k]
                Sigma[feat_1, feat_2] <- rho_high
                Sigma[feat_2, feat_1] <- rho_high
            }
        }
    }

    stopifnot(is.numeric(Sigma))
    stopifnot(is.matrix(Sigma))
    stopifnot(nrow(Sigma) == p & ncol(Sigma) == p)
    stopifnot(all(Sigma == t(Sigma)))

    return(Sigma)
}

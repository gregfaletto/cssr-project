# Generated from create-cssr.Rmd: do not edit by hand

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
#' @param rho Integer or numeric; the covariance of the proxies in each cluster
#' with the latent variable (and each other). Note that the correlation between
#' the features in the cluster will be rho/var. rho cannot equal 0.
#' @param var Integer or numeric; the variance of all of the observed features
#' in X (both the proxies for the latent variables and the k_unclustered other
#' features). var cannot equal 0.
#' @return A `p` x `p` numeric matrix representing the covariance matrix for
#' the latent features, the associated proxies, and the remaining features. All
#' features not in a block will be independent from each other and the blocks
#' and have variance var.
#' @author Gregory Faletto, Jacob Bien
makeCovarianceMatrix <- function(p, nblocks, block_size, rho, var) {
    # Check inputs

    stopifnot(p >= nblocks*block_size)
    stopifnot(abs(rho) <= abs(var))
    stopifnot(nblocks >= 1)
    stopifnot(rho != 0)
    stopifnot(var != 0)
    stopifnot(block_size >= 2)

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
                Sigma[feat_1, feat_2] <- rho
                Sigma[feat_2, feat_1] <- rho
            }
        }
    }
    stopifnot(is.numeric(Sigma))
    stopifnot(nrow(Sigma) == p & ncol(Sigma) == p)
    stopifnot(all(Sigma == t(Sigma)))

    return(Sigma)
}

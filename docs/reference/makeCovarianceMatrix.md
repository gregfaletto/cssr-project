# Generate covariance matrix for simulated clustered data

Generate covariance matrix for simulated clustered data

## Usage

``` r
makeCovarianceMatrix(p, nblocks, block_size, rho, var)
```

## Arguments

- p:

  Integer or numeric; the total number of features in the covariance
  matrix to be created, including latent features, the associated noisy
  proxies with each latent feature, and other (weak signal and noise)
  features.

- block_size:

  Integer or numeric; for each of the n_blocks latent variables, the
  covariance matrix will include the original latent feature plus
  block_size - 1 noisy proxies that are correlated with the latent
  variable.

- rho:

  Integer or numeric; the covariance of the proxies in each cluster with
  the latent variable (and each other). Note that the correlation
  between the features in the cluster will be rho/var. rho cannot equal
  0.

- var:

  Integer or numeric; the variance of all of the observed features in X
  (both the proxies for the latent variables and the k_unclustered other
  features). var cannot equal 0.

- n_blocks:

  Integer or numeric; the number of latent variables in the data, each
  of which is associated with an observed cluster in X. Must be at least
  1.

## Value

A `p` x `p` numeric matrix representing the covariance matrix for the
latent features, the associated proxies, and the remaining features. All
features not in a block will be independent from each other and the
blocks and have variance var.

## Author

Gregory Faletto, Jacob Bien

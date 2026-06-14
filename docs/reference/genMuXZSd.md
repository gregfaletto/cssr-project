# Generate observed and latent variables along with conditional mean

Generate observed and latent variables along with conditional mean

## Usage

``` r
genMuXZSd(
  n,
  p,
  beta,
  Sigma,
  blocked_dgp_vars,
  latent_vars,
  block_size,
  n_blocks = 1,
  snr = NA,
  sigma_eps_sq = NA
)
```

## Arguments

- n:

  Integer or numeric; the number of observations to generate. (The
  generated X and Z will have n rows, and the generated y and mu will
  have length n.)

- p:

  Integer or numeric; the number of observed features (the generated X
  will have p columns).

- beta:

  A numeric or integer vector of length `p` + sig_blocks containing the
  coefficients for the true model for y.

- Sigma:

  A (`p` + n_blocks) x (`p` + n_blocks) numeric matrix representing the
  covariance matrix for the latent features, the associated proxies, and
  the remaining features.

- blocked_dgp_vars:

  An integer vector of length sig_blocks containing the indices of the
  features corresponding to the latent features that have nonzero
  coefficient beta_high in the true model for y.

- latent_vars:

  An integer vector of length n_blocks containing the indices of all of
  the latent features.

- block_size:

  Integer or numeric; for each of the n_blocks latent variables, X will
  contain block_size noisy proxies that are correlated with the latent
  variable.

- n_blocks:

  Integer or numeric; the number of latent variables to generate, each
  of which will be associated with an observed cluster in X. Must be at
  least 1. Default is 1.

- snr:

  Integer or numeric; the signal-to-noise ratio of the response y. If
  sigma_eps_sq is not specified, the variance of the noise in y will be
  calculated using the formula sigma_eps_sq = sum(mu^2)/(n \* snr). Only
  one of snr and sigma_eps_sq must be specified. Default is NA.

- sigma_eps_sq:

  Integer or numeric; the variance on the noise added to y. Only one of
  snr and sigma_eps_sq must be specified. Default is NA.

## Value

A named list with the following elements:

- X:

  An `n` x `p` numeric matrix containing the observed proxies for the
  latent variables as well as the observed unblocked (iid) variables.

- mu:

  A length `n` numeric vector; the expected response given X, Z, and the
  true coefficient vector (equal to y minus the added noise).

- z:

  An `n` x n_blocks numeric matrix containing the n_blocks latent
  variables. Note that (X, z) is multivariate Gaussian.

- sd:

  Numeric; the standard deviation of the noise added to mu to get y
  (calculated either from snr or sigma_eps_sq).

## Author

Gregory Faletto, Jacob Bien

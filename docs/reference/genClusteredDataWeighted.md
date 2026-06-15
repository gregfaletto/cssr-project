# Generate randomly sampled data including noisy observations of latent variables, where proxies differ in their relevance (noise level)

Generate a data set including latent features Z, observed features X
(which may include noisy or noiseless observations of the latent
features in Z), an observed response y which is a linear model of
features from Z and X as well as independent mean zero noise, and mu
(the responses from y without the added noise). Data is generated in the
same way as in the simulations from Section 5.3 of Faletto and Bien
(2022).

## Usage

``` r
genClusteredDataWeighted(
  n,
  p,
  k_unclustered,
  cluster_size,
  n_strong_cluster_vars,
  n_clusters = 1,
  sig_clusters = 1,
  rho_high = 0.9,
  rho_low = 0.5,
  beta_latent = 1.5,
  beta_unclustered = 1,
  snr = as.numeric(NA),
  sigma_eps_sq = as.numeric(NA)
)
```

## Arguments

- n:

  Integer or numeric; the number of observations to generate. (The
  generated X and Z will have n rows, and the generated y and mu will
  have length n.)

- p:

  Integer or numeric; the number of features to generate. The generated
  X will have p columns.

- k_unclustered:

  Integer or numeric; the number of features in X that will have nonzero
  coefficients in the true model for y among those features not
  generated from the n_clusters latent variables (called "weak signal"
  features in the simulations from Faletto and Bien 2022). The
  coefficients on these features will be determined by beta_unclustered.
  Must be at least 1.

- cluster_size:

  Integer or numeric; for each of the n_clusters latent variables, X
  will contain cluster_size noisy proxies that are correlated with the
  latent variable.

- n_strong_cluster_vars:

  Integer or numeric; among the cluster_size proxies in each cluster,
  the first n_strong_cluster_vars will have a high covariance (rho_high)
  with the latent variable and the next cluster_size -
  n_strong_cluster_vars will have a low covariance (rho_low) with the
  latent variable.

- n_clusters:

  Integer or numeric; the number of latent variables to generate, each
  of which will be associated with an observed cluster in X. Must be at
  least 1. Default is 1.

- sig_clusters:

  Integer or numeric; the number of generated latent features that will
  have nonzero coefficients in the true model for y (all of them will
  have coefficient beta_latent). Must be less than or equal to
  n_clusters. Default is 1.

- rho_high:

  Integer or numeric; the covariance of the "strong proxies" in each
  cluster with the latent variable (and each other). Note that the
  correlation between the "strong proxy" features in the cluster will be
  rho_high/var. rho_high cannot equal 0 and must be at least as large as
  rho_low. Default is 0.9.

- rho_low:

  Integer or numeric; the covariance of the "weak proxies" in each
  cluster with the latent variable (and each other). Note that the
  correlation between the "weak proxy" features in the cluster will be
  rho_low/var. rho_low cannot equal 0 and must be no larger than
  rho_high. Default is 0.5.

- beta_latent:

  Integer or numeric; the coefficient used for all sig_clusters latent
  variables that have nonzero coefficients in the true model for y.
  Can't equal 0. Default is 1.5.

- beta_unclustered:

  Integer or numeric; the maximum coefficient in the model for y among
  the k_unclustered features in X not generated from the latent
  variables. The coefficients of the features will be
  beta_unclustered/sqrt(1:k_unclustered). Can't equal 0. Default is 1.

- snr:

  Integer or numeric; the signal-to-noise ratio of the response y. If
  sigma_eps_sq is not specified, the variance of the noise in y will be
  calculated using the formula sigma_eps_sq = sum(mu^2)/(n \* snr). Only
  one of snr and sigma_eps_sq must be specified. Default is NA.

- sigma_eps_sq:

  Integer or numeric; the variance on the noise added to y
  (non-negative; 0 gives noiseless y). Only one of snr and sigma_eps_sq
  must be specified. Default is NA.

## Value

A list of the following elements.

- X:

  An n x p numeric matrix of n observations from a p-dimensional
  multivariate normal distribution generated using the specified
  parameters. The first n_clusters times cluster_size features will be
  the clusters of features correlated with the n_clusters latent
  variables. The next k_unclustered features will be the "weak signal"
  features, and the remaining p - n_clusters\*cluster_size -
  k_unclustered features will be the unclustered noise features.

- y:

  A length n numeric vector; the response generated from X, the latent
  features from Z, and the coefficient vector, along with additive
  noise.

- Z:

  The latent features; either a numeric vector (if n_clusters \> 1) or a
  numeric matrix (if n_clusters \> 1). Note that (X, Z) is multivariate
  Gaussian.

- mu:

  A length `n` numeric vector; the expected response given X, Z, and the
  true coefficient vector (equal to y minus the added noise).

## References

Faletto, G., & Bien, J. (2022). Cluster Stability Selection. *arXiv
preprint arXiv:2201.00494*. <https://arxiv.org/abs/2201.00494>.

## Author

Gregory Faletto, Jacob Bien

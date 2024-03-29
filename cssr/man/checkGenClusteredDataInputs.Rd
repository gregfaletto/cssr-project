% Generated by roxygen2: do not edit by hand
% Please edit documentation in _main.Rmd.
\name{checkGenClusteredDataInputs}
\alias{checkGenClusteredDataInputs}
\title{Check inputs to genClusteredData}
\usage{
checkGenClusteredDataInputs(
  p,
  k_unclustered,
  cluster_size,
  n_clusters,
  sig_clusters,
  rho,
  beta_latent,
  beta_unclustered,
  snr,
  sigma_eps_sq
)
}
\arguments{
\item{p}{Integer or numeric; the number of features to generate. The
generated X will have p columns.}

\item{k_unclustered}{Integer or numeric; the number of features in X that
will have nonzero coefficients in the true model for y among those features
not generated from the n_clusters latent variables (called "weak signal"
features in the simulations from Faletto and Bien 2022). The coefficients on
these features will be determined by beta_unclustered.}

\item{cluster_size}{Integer or numeric; for each of the n_clusters latent
variables, X will contain cluster_size noisy proxies that are correlated with
the latent variable.}

\item{n_clusters}{Integer or numeric; the number of latent variables to
generate, each of which will be associated with an observed cluster in X.
Must be at least 1. Default is 1.}

\item{sig_clusters}{Integer or numeric; the number of generated latent
features that will have nonzero coefficients in the true model for y (all of
them will have coefficient beta_latent). Must be less than or equal to
n_clusters. Default is 1.}

\item{rho}{Integer or numeric; the covariance of the proxies in each cluster
with the latent variable (and each other). Note that the correlation between
the features in the cluster will be rho/var. Can't equal 0. Default is 0.9.}

\item{beta_latent}{Integer or numeric; the coefficient used for all
sig_clusters latent variables that have nonzero coefficients in the true
model for y. Can't equal 0. Default is 1.5.}

\item{beta_unclustered}{Integer or numeric; the maximum coefficient in the
model for y among the k_unclustered features in X not generated from the
latent variables. The coefficients of the features will be
beta_unclustered/sqrt(1:k_unclustered). Can't equal 0. Default is 1.}

\item{snr}{Integer or numeric; the signal-to-noise ratio of the response
y. If sigma_eps_sq is not specified, the variance of the noise in y will be
calculated using the formula sigma_eps_sq = sum(mu^2)/(n * snr). Only one of
snr and sigma_eps_sq must be specified. Default is NA.}

\item{sigma_eps_sq}{Integer or numeric; the variance on the noise added
to y. Only one of snr and sigma_eps_sq must be specified. Default is NA.}
}
\value{
A list of the following elements. \item{X}{An n x p numeric matrix of
n observations from a p-dimensional multivariate normal distribution
generated using the specified parameters. The first n_clusters times
cluster_size features will be the clusters of features correlated with the
n_clusters latent variables. The next k_unclustered features will be the
"weak signal" features, and the remaining p - n_clusters*cluster_size -
k_unclustered features will be the unclustered noise features.} \item{y}{A
length n numeric vector; the response generated from X, the latent features
from Z, and the coefficient vector, along with additive noise.} \item{Z}{The
latent features; either a numeric vector (if n_clusters > 1) or a numeric
matrix (if n_clusters > 1). Note that (X, Z) is multivariate Gaussian.}
item{mu}{A length \code{n} numeric vector; the expected response given X, Z, and
the true coefficient vector (equal to y minus the added noise).}
}
\description{
Check inputs to genClusteredData
}
\author{
Gregory Faletto, Jacob Bien
}
# Converts the provided design matrix to an appropriate format for either the protolasso or the cluster representative lasso.

Creates design matrix for glmnet by dealing with clusters (for
type="protolasso", discards all cluster members except prototype; for
type="clusterRepLasso", replaces all cluster members with a simple
average of all the cluster members).

## Usage

``` r
getXglmnet(x, clusters, type, prototypes = NA)
```

## Arguments

- x:

  A numeric matrix; the provided matrix with n observations and p
  features.

- clusters:

  A named list where each entry is an integer vector of indices of
  features that are in a common cluster. (The length of list clusters
  should be equal to the number of clusters.) All identified clusters
  should be non-overlapping. All features should appear in exactly one
  cluster (any unclustered features should be put in their own "cluster"
  of size 1).

- type:

  Character; "protolasso" for the protolasso or "clusterRepLasso" for
  the cluster representative lasso.

- prototypes:

  Only required for type "protolasso". An integer vector whose length is
  equal to the number of clusters. Entry i should be the prototype for
  cluster i (the feature belonging to cluster i that is most highly
  correlated with y; see Reid and Tibshirani 2016).

## Value

A numeric matrix; the design matrix as required for the protolasso or
cluster representative lasso, prepared for input to glmnet.

## References

Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
testing using cluster prototypes. *Biostatistics*, 17(2), 364–376.
<https://doi.org/10.1093/biostatistics/kxv049>.

## Author

Gregory Faletto, Jacob Bien

# Formats clusters in standardized way, optionally estimating cluster prototypes

Formats clusters in standardized way, optionally estimating cluster
prototypes

## Usage

``` r
formatClusters(
  clusters = NA,
  p = -1,
  clust_names = NA,
  get_prototypes = FALSE,
  x = NA,
  y = NA,
  R = NA
)
```

## Arguments

- clusters:

  Either an integer vector of a list of integer vectors; each vector
  should contain the indices of a cluster of features. (If there is only
  one cluster, clusters can either be a list of length 1 or simply an
  integer vector.) If clusters is specified then R is ignored.

- p:

  integer or numeric; the numbe of features in x (should match ncol(x),
  if x is provided)

- clust_names:

  A character vector of the names of the clusters in clusters.

- get_prototypes:

  Logical: if TRUE, will identify prototype from each cluster (the
  feature from each cluster that is most correlated with the response)
  for the protolasso. In this case, x and y must be provided.

- x:

  n x p numeric matrix; design matrix. Only needs to be provided if
  get_prototypes is TRUE.

- y:

  Numeric response vector; only needs to be provided if get_prototypes
  is TRUE. Note: in general, the css function does not require y to be a
  numeric vector, because the provided fitfun could use a different form
  of y (for example, a categorical response variable). However, y must
  be numeric in order to provide prototypes because the prototypes are
  determined using the correlation between cluster members (columns
  of x) and y.

- R:

  Numeric p x p matrix; not currently used. Entry ij contains the
  "substitutive value" of feature i for feature j (diagonal must consist
  of ones, all entries must be between 0 and 1, and matrix must be
  symmetric)

## Value

A named list with the following elements:

- clusters:

  A named list where each entry is an integer vector of indices of
  features that are in a common cluster. (The length of list clusters is
  equal to the number of clusters.) All identified clusters are
  non-overlapping. All features appear in exactly one cluster (any
  unclustered features will be put in their own "cluster" of size 1).

- multiple:

  Logical; TRUE if there is more than one cluster of size greater than
  1, FALSE otherwise.

- prototypes:

  only returned if get_prototypes=TRUE. An integer vector whose length
  is equal to the number of clusters. Entry i is the index of the
  feature belonging to cluster i that is most highly correlated with y
  (that is, the prototype for the cluster, as in the protolasso; see
  Reid and Tibshirani 2016).

## References

Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
testing using cluster prototypes. *Biostatistics*, 17(2), 364–376.
<https://doi.org/10.1093/biostatistics/kxv049>.

## Author

Gregory Faletto, Jacob Bien

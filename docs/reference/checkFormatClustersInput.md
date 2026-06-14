# Helper function to ensure that the inputs to formatClusters are as expected

Helper function to ensure that the inputs to formatClusters are as
expected

## Usage

``` r
checkFormatClustersInput(clusters, p, clust_names, get_prototypes, x, y, R)
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

A list of integer vectors; each vector will contain the indices of a
cluster of features. Any duplicated clusters provided in the input will
be removed.

## Author

Gregory Faletto, Jacob Bien

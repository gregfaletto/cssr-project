# Estimate prototypes from a single cluster

Takes in a single cluster, x, and y and returns an integer of the index
of the feature prototype (the feature from the cluster most correlated
with the response).

## Usage

``` r
identifyPrototype(cluster_members_i, x, y)
```

## Arguments

- cluster_members_i:

  An integer vector of indices of features that are in a common cluster.
  Must have length at least 2.

- x:

  n x p numeric matrix; design matrix.

- y:

  Numeric response vector. Note: in general, the css function does not
  require y to be a numeric vector, because the provided fitfun could
  use a different form of y (for example, a categorical response
  variable). However, y must be numeric in order to provide prototypes
  because the prototypes are determined using the correlation between
  cluster members (columns of x) and y.

## Value

integer; the index of the feature identified as the prototype for the
cluster.

## Author

Gregory Faletto, Jacob Bien

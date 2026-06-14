# Estimate prototypes from a list of clusters

Takes in list of clusters, x, and y and returns an integer vector (of
length equal to the number of clusters) of the indices of the feature
prototypes (the features from each cluster most correlated with the
response).

## Usage

``` r
getPrototypes(clusters, x, y)
```

## Arguments

- clusters:

  A list where each entry is an integer vector of indices of features
  that are in a common cluster. (The length of list clusters must be
  equal to the number of clusters.) All identified clusters must be
  non-overlapping. Must only include clusters of size 2 or larger.

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

An integer vector of the same length as clusters. Entry j is the index
of the feature identified as the prototype for cluster j.

## Author

Gregory Faletto, Jacob Bien

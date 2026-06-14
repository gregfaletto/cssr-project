# Get cluster selection matrix

Given a matrix of feature selection indicator variables and a list of
clusters of features, returns a matrix of cluster indicator variables.

## Usage

``` r
getClusterSelMatrix(clusters, res)
```

## Arguments

- clusters:

  A named list where each entry is an integer vector of indices of
  features that are in a common cluster, as in the output of
  formatClusters. (The length of list clusters is equal to the number of
  clusters.) All identified clusters must be non-overlapping, and all
  features must appear in exactly one cluster (any unclustered features
  should be in their own "cluster" of size 1).

- res:

  A binary integer matrix. `res[i, j]` = 1 if feature j was selected on
  subsample i and equals 0 otherwise, as in the output of
  [`getSelMatrix()`](getSelMatrix.md). (That is, each row is a selected
  set.)

## Value

A binary integer matrix with the same number of rows as res and
length(clusters) columns. Entry i, j is 1 if at least one member of
cluster j was selected on subsample i, and 0 otherwise.

## Author

Gregory Faletto, Jacob Bien

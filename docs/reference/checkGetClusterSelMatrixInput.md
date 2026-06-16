# Helper function to check inputs to getClusterSelMatrix function

Helper function to check inputs to getClusterSelMatrix function

## Usage

``` r
checkGetClusterSelMatrixInput(clusters, res)
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
  subsample i and equals 0 otherwise, as in the output of getSelMatrix.
  (That is, each row is a selected set.)

## Value

No return value; called for the side effect of erroring if the inputs to
getClusterSelMatrix are invalid.

## Author

Gregory Faletto, Jacob Bien

# Helper function to confirm that clusters input to css is as expected

Helper function to confirm that clusters input to css is as expected

## Usage

``` r
checkCssClustersInput(clusters)
```

## Arguments

- clusters:

  A list of integer vectors; each vector should contain the indices of a
  cluster of features (a subset of `1:p`). (If there is only one
  cluster, clusters can either be a list of length 1 or an integer
  vector.) All of the provided clusters must be non-overlapping. Every
  feature not appearing in any cluster will be assumed to be unclustered
  (that is, they will be treated as if they are in a "cluster"
  containing only themselves). If clusters is a list of length 0 (or a
  list only containing clusters of length 1), then [`css()`](css.md)
  returns the same results as stability selection (so the returned
  `feat_sel_mat` will be identical to `clus_sel_mat`). Names for the
  clusters will be needed later; any clusters that are not given names
  in the provided list will be given names automatically by
  [`css()`](css.md). \#' CAUTION: if the provided X is a data.frame that
  contains a categorical feature with more than two levels, then the
  resulting matrix made from model.matrix will have a different number
  of columns than the provided data.frame, some of the feature numbers
  will change, and the clusters argument will not work properly (in the
  current version of the package). To get correct results in this case,
  please use model.matrix to convert the data.frame to a numeric matrix
  on your own, then provide this matrix and cluster assignments with
  respect to this matrix. Default is
  [`list()`](https://rdrr.io/r/base/list.html) (so no clusters are
  specified).

## Value

Same as the input, but all of the clusters will be coerced to integers.

## Author

Gregory Faletto, Jacob Bien

# Helper function to check that output of getSelectedClusters is as expected

Helper function to check that output of getSelectedClusters is as
expected

## Usage

``` r
checkGetSelectedClustersOutput(
  selected_clusts,
  selected_feats,
  weights,
  n_clusters,
  p
)
```

## Arguments

- selected_clusts:

  A named numeric vector containing the selection proportions for the
  selected clusters. The name of each entry is the name of the
  corresponding cluster.

- selected_feats:

  A named integer vector; the indices of the features with nonzero
  weights from all of the selected clusters.

- weights:

  A named list of the same length as the number of selected clusters.
  Each list element `weights[[j]]` is a numeric vector of the weights to
  use for the jth selected cluster, and it has the same name as the
  cluster it corresponds to.

- n_clusters:

  Integer; the number of clusters in the data (upper bound for the
  length of selected_clusts)

- p:

  Integer; number of features in the data (all selected_feats should be
  in 1:p)

## Author

Gregory Faletto, Jacob Bien

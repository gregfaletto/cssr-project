# Calculate weights for members of a cluster using selection proportions

Given a cluster of features, the selection proportions for each cluster
member, and a specified weighting scheme, calculate the appropriate
weights for the cluster.

## Usage

``` r
getClustWeights(cluster_i, weighting, feat_sel_props)
```

## Arguments

- cluster_i:

  An integer vector containing the indices of the members of a cluster.

- weighting:

  Character; determines how to calculate the weights for individual
  features within the selected clusters. Only those features with
  nonzero weight within the selected clusters will be returned. Must be
  one of "sparse", "weighted_avg", or "simple_avg'. For "sparse", all
  the weight is put on the most frequently selected individual cluster
  member (or divided equally among all the clusters that are tied for
  the top selection proportion if there is a tie). For "weighted_avg",
  only the features within a selected cluster that were themselves
  selected on at least one subsample will have nonzero weight. For
  "simple_avg", each cluster member gets equal weight regardless of the
  individual feature selection proportions (that is, all cluster members
  within each selected cluster will be returned.). See Faletto and
  Bien (2022) for details.

- feat_sel_props:

  A numeric vector of selection proportions corresponding to each of the
  p features.

## Value

A numeric vector of the same length as cluster_i containing the weights
corresponding to each of the features in cluster_i. The weights will all
be nonnegative and sum to 1.

## Author

Gregory Faletto, Jacob Bien

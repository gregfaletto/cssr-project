# Obtain a selected set of clusters and features

Generate sets of selected clusters and features from cluster stability
selection.

## Usage

``` r
getCssSelections(
  css_results,
  weighting = "sparse",
  cutoff = 0,
  min_num_clusts = 1,
  max_num_clusts = NA
)
```

## Arguments

- css_results:

  An object of class "cssr" (the output of the function css).

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
  Bien (2022) for details. Default is "sparse".

- cutoff:

  Numeric; getCssSelections will select and return only of those
  clusters with selection proportions equal to at least cutoff. Must be
  between 0 and 1. Default is 0 (in which case either all clusters are
  selected, or max_num_clusts are selected, if max_num_clusts is
  specified).

- min_num_clusts:

  Integer or numeric; the minimum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns fewer than
  min_num_clusts clusters, the cutoff will be increased until at least
  min_num_clusts clusters are selected.) Default is 1.

- max_num_clusts:

  Integer or numeric; the maximum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns more than
  max_num_clusts clusters, the cutoff will be decreased until at most
  max_num_clusts clusters are selected.) Default is NA (in which case
  max_num_clusts is ignored).

## Value

A named list with two items.

- selected_clusts:

  A named list of integer vectors; each vector contains the indices of
  the features in one of the selected clusters.

- selected_feats:

  A named integer vector; the indices of the features with nonzero
  weights from all of the selected clusters.

- weights:

  A named list of the same length as selected_clusts. Each list element
  `weights[[j]]` is a numeric vector of the weights to use for the jth
  selected cluster, and it has the same name as the cluster it
  corresponds to.

## References

Faletto, G., & Bien, J. (2022). Cluster Stability Selection. *arXiv
preprint arXiv:2201.00494*. <https://arxiv.org/abs/2201.00494>.

## Author

Gregory Faletto, Jacob Bien

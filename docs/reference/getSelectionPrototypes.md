# Identify selection prototypes from selected clusters

Takes in list of selected clusters and returns an integer vector of the
indices of the features that were most frequently selected from each
cluster

## Usage

``` r
getSelectionPrototypes(css_results, selected_clusts)
```

## Arguments

- css_results:

  An object of class "cssr" (the output of the function css).

- selected_clusts:

  A list of integer vectors; each vector must contain the indices of
  features in a cluster.

## Value

An integer vector (of length equal to the number of clusters) of the
indices of the feature prototypes (the features from each cluster that
were selected the most often individually by the base method in cluster
stability selection). In the case of a tie, the tie is broken by
choosing the feature most correlated with the response in the full data
set provided to css.

## Author

Gregory Faletto, Jacob Bien

# Obtain a design matrix of cluster representatives

Takes a matrix of observations from the original feature space and
returns a matrix of representatives from the selected clusters based on
the results of cluster stability selection.

## Usage

``` r
getCssDesign(
  css_results,
  newX = NA,
  weighting = "weighted_avg",
  cutoff = 0,
  min_num_clusts = 1,
  max_num_clusts = NA
)
```

## Arguments

- css_results:

  An object of class "cssr" (the output of the function css).

- newX:

  A numeric matrix (preferably) or a data.frame (which will be coerced
  internally to a matrix by the function model.matrix) containing the
  data that will be used to generate the design matrix of cluster
  representatives. Must contain the same features (in the same number of
  columns) as the X matrix provided to css, and if the columns of newX
  are labeled, the names must match the variable names provided to css.
  newX may be omitted if train_inds were provided to css to set aside
  observations for model estimation. If this is the case, then when newX
  is omitted getCssDesign will return a design matrix of cluster
  representatives formed from the train_inds observations from the
  matrix X provided to css. (If no train_inds were provided to css, newX
  must be provided to getCssDesign.) Default is NA. Must not contain
  missing (`NA`) values.

- weighting:

  Character; determines how to calculate the weights to combine features
  from the selected clusters into weighted averages, called cluster
  representatives. Must be one of "sparse", "weighted_avg", or
  "simple_avg'. For "sparse", all the weight is put on the most
  frequently selected individual cluster member (or divided equally
  among all the clusters that are tied for the top selection proportion
  if there is a tie). For "weighted_avg", the weight used for each
  cluster member is calculated in proportion to the individual selection
  proportions of each feature. For "simple_avg", each cluster member
  gets equal weight regardless of the individual feature selection
  proportions (that is, the cluster representative is just a simple
  average of all the cluster members). See Faletto and Bien (2022) for
  details. Default is "weighted_avg".

- cutoff:

  Numeric; getCssDesign will only include those clusters with selection
  proportions equal to at least cutoff. Must be between 0 and 1. Default
  is 0 (in which case either all clusters are used, or max_num_clusts
  are used, if max_num_clusts is specified).

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

A design matrix with either nrow(newX) (or length(train_inds), if
train_inds was provided to css and newX was not provided to
getCssDesign) observations and number of columns equal to the number of
selected clusters, containing the cluster representatives for each
cluster.

## Author

Gregory Faletto, Jacob Bien

## Examples

``` r
set.seed(1)
train <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
  cluster_size = 4, n_clusters = 1, snr = 3)
newdata <- genClusteredData(n = 10, p = 11, k_unclustered = 2,
  cluster_size = 4, n_clusters = 1, snr = 3)
clusters <- list(cluster1 = 1:4)
res <- css(X = train$X, y = train$y, lambda = 0.01, clusters = clusters,
  B = 10)
X_design <- getCssDesign(res, newX = newdata$X)
dim(X_design)
#> [1] 10  8
```

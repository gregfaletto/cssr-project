# Obtain a selected set of clusters and features using cluster stability selection

Takes in data X and y and returns a set of clusters (and a set of
features) that are useful for predicting y from the data in X. This is a
wrapper function for css and getCssSelections. Using cssSelect is
simpler, but it has fewer options, and it executes the full
(computationally expensive) subsampling procedured every time it is
called. In contrast, css can be called just once, and then
getCssSelections can quickly return results using different values of
cutoff, max_num_clusts, etc. from the calculations done in one call to
css.

## Usage

``` r
cssSelect(
  X,
  y,
  clusters = list(),
  lambda = NA,
  cutoff = NA,
  max_num_clusts = NA,
  auto_select_size = TRUE,
  alpha = 1
)
```

## Arguments

- X:

  An n x p numeric matrix (preferably) or a data.frame (which will be
  coerced internally to a matrix by the function model.matrix)
  containing the p \>= 2 features/predictors. Must not contain missing
  (`NA`) values.

- y:

  A length-n numeric vector containing the responses; `y[i]` is the
  response corresponding to observation `X[i, ]`. (Note that for the css
  function, y does not have to be a numeric response, but for this
  function, the underlying selection procedure is the lasso, so y must
  be a real-valued response.)

- clusters:

  Optional; either an integer vector of a list of integer vectors; each
  vector should contain the indices of a cluster of features (a subset
  of 1:p). (If there is only one cluster, clusters can either be a list
  of length 1 or an integer vector.) All of the provided clusters must
  be non-overlapping. Any identical (duplicated) clusters will be
  removed, keeping the first occurrence and its name. Every feature not
  appearing in any cluster will be assumed to be unclustered (that is,
  they will be treated as if they are in a "cluster" containing only
  themselves). If clusters is a list of length 0 (or a list only
  containing clusters of length 1), then css() returns the same results
  as stability selection (so feat_sel_mat will be identical to
  clus_sel_mat). Names for the clusters will be needed later; any
  clusters that are not given names in the list clusters will be given
  names automatically by css. CAUTION: if the provided X is a data.frame
  that contains a categorical feature with more than two levels, then
  the resulting matrix made from model.matrix will have a different
  number of columns than the provided data.frame, some of the feature
  numbers will change, and the clusters argument will not work properly
  (in the current version of the package). To get correct results in
  this case, please use model.matrix to convert the data.frame to a
  numeric matrix on your own, then provide this matrix and cluster
  assignments with respect to this matrix. Default is list() (so no
  clusters are specified, and every feature is assumed to be in a
  "cluster" containing only itself).

- lambda:

  Optional; the tuning parameter to be used by the lasso for feature
  selection in each subsample. If lambda is not provided, cssSelect will
  choose one automatically by cross-validation. Default is NA.

- cutoff:

  Numeric; cssSelect will only select those clusters with selection
  proportions equal to at least cutoff. Must be between 0 and 1. Default
  is NA (in which case max_num_clusts are used).

- max_num_clusts:

  Integer or numeric; the maximum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns more than
  max_num_clusts clusters, the cutoff will be raised until at most
  max_num_clusts clusters are selected.) Default is NA (in which case
  either cutoff is used to choose the number of clusters, or if cutoff
  was also unspecified, cssSelect chooses max_num_clusts by
  cross-validation).

- auto_select_size:

  Logical; if TRUE, then max_num_clusts will be automatically estimated
  using the lasso with cross-validation. Default is TRUE, though this
  argument is ignored if either cutoff or max_num_clusts is provided.
  (If desired output is to return all clusters, you should set
  auto_select_size to FALSE and do not provide cutoff or
  max_num_clusts.)

- alpha:

  Numeric; the elastic net mixing parameter. Must be in `(0, 1]`. Drives
  both the choice of lambda (when lambda is not provided) and the
  elastic net fit used for feature selection in each subsample. Default
  is 1 (in which case the penalty is the lasso).

## Value

A named list with three items.

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

## Author

Gregory Faletto, Jacob Bien

## Examples

``` r
set.seed(1)
data <- genClusteredData(n = 80, p = 11, k_unclustered = 2,
  cluster_size = 4, n_clusters = 1, snr = 3)
clusters <- list(cluster1 = 1:4)
res <- cssSelect(X = data$X, y = data$y, clusters = clusters)
res$selected_feats
#> [1] 2 5 6
```

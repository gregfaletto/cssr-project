# Select features via the cluster representative lasso (Bühlmann et. al. 2013)

Select features via the cluster representative lasso (Bühlmann et. al.
2013)

## Usage

``` r
clusterRepLasso(X, y, clusters = list(), nlambda = 100)
```

## Arguments

- X:

  An n x p numeric matrix (preferably) or a data.frame (which will be
  coerced internally to a matrix by the function model.matrix)
  containing p \>= 2 features/predictors. Must not contain missing
  (`NA`) values.

- y:

  The response; A length n numeric (or integer) real-valued vector.

- clusters:

  A list of integer vectors; each vector should contain the indices of a
  cluster of features (a subset of 1:p). (If there is only one cluster,
  clusters can either be a list of length 1 or an integer vector.) All
  of the provided clusters must be non-overlapping. Every feature not
  appearing in any cluster will be assumed to be unclustered (that is,
  they will be treated as if they are in a "cluster" containing only
  themselves). CAUTION: if the provided X is a data.frame that contains
  a categorical feature with more than two levels, then the resulting
  matrix made from model.matrix will have a different number of columns
  than the provided data.frame, some of the feature numbers will change,
  and the clusters argument will not work properly (in the current
  version of the package). To get correct results in this case, please
  use model.matrix to convert the data.frame to a numeric matrix on your
  own, then provide this matrix and cluster assignments with respect to
  this matrix. Default is list() (so no clusters are specified).

- nlambda:

  Integer; the number of lambda values to use in the lasso fit for the
  cluster representative lasso. Default is 100 (following the default
  for glmnet). For now, nlambda must be at least 2 (using a single
  lambda is not supported).

## Value

A list with three elements.

- selected_sets:

  A list of integer vectors. Entry k of this list contains a selected
  set (an integer vector) of size k yielded by the lasso–each member of
  the set is the index of a single feature from a cluster selected by
  the cluster representative lasso (the prototype from that cluster–the
  cluster member most highly correlated with y). (If no set of size k
  was selected, entry k will be empty.)

- selected_clusts_list:

  A list; each element of the list is a named list of selected clusters.
  (That is, if a selected set of size k was yielded by the cluster
  representative lasso, then `selected_clusts_list[[k]]` is a named list
  of length k, where each member of the list is an integer vector of
  cluster members. Note that `selected_clusts_lists[[k]][[j]]` will be
  the cluster that contains feature `selected_sets[[k]][j]`.)

- beta:

  The beta output from glmnet when the lasso was estimated on a matrix
  of cluster representatives (simple averages of the cluster members).
  (See documentation for the function glmnet from the glmnet package for
  details.) The rows of beta correspond, in order, to the clusters in
  the internally reordered clusters list (the provided clusters in the
  supplied order, followed by a singleton cluster for each unclustered
  feature in ascending feature-index order). Each row name is the
  cluster's name–the provided name for a supplied cluster, or an
  automatically assigned `c<position>` name (position in the reordered
  list, not the feature index) for a singleton; a selected cluster's row
  name coincides with its entry name in `selected_clusts_list[[k]]`.

## References

Bühlmann, P., Rütimann, P., van de Geer, S., & Zhang, C. H. (2013).
Correlated variables in regression: Clustering and sparse estimation.
*Journal of Statistical Planning and Inference*, 143(11), 1835–1858.
<https://doi.org/10.1016/j.jspi.2013.05.019>.  
Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization
Paths for Generalized Linear Models via Coordinate Descent. *Journal of
Statistical Software*, 33(1) 1-22. URL
<https://www.jstatsoft.org/v33/i01/>.

## Author

Gregory Faletto, Jacob Bien

## Examples

``` r
set.seed(1)
data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
  cluster_size = 4, n_clusters = 1, snr = 3)
clusters <- list(cluster1 = 1:4)
res <- clusterRepLasso(X = data$X, y = data$y, clusters = clusters)
str(res, max.level = 1)
#> List of 3
#>  $ selected_sets       :List of 8
#>  $ selected_clusts_list:List of 8
#>  $ beta                :Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
```

# Extracts selected clusters and cluster prototypes from the glmnet lasso output

Extracts selected clusters and cluster prototypes from the glmnet lasso
output

## Usage

``` r
getClusterSelsFromGlmnet(lasso_sets, clusters, prototypes, feat_names)
```

## Arguments

- lasso_sets:

  A list of integer vectors. Each vector represents a set of features
  selected by the lasso for a given value of the penalty parameter
  lambda.

- clusters:

  A named list where each entry is an integer vector of indices of
  features that are in a common cluster. (The length of list clusters is
  equal to the number of clusters.) All identified clusters must be
  non-overlapping. All features appear in exactly one cluster (any
  unclustered features must be in their own "cluster" of size 1).

- prototypes:

  An integer vector whose length must be equal to the number of
  clusters. Entry i should be the index of the feature belonging to
  cluster i that is most highly correlated with y (that is, the
  prototype for the cluster, as in the protolasso; see Reid and
  Tibshirani 2016).

- feat_names:

  Character vector; the names of the features in X. (If the X provided
  to protolasso or clusterRepLasso did not have feature names,
  feat_names will be NA.)

## Value

A list containing the following items:

- selected_sets:

  A list of integer vectors. Entry k of this list contains a selected
  set of size k yielded by glmnet–each member of the set is the index of
  a single feature from a cluster selected by either the protolasso or
  the cluster representative lasso (the prototype from that cluster–the
  cluster member most highly correlated with y). (If no set of size k
  was selected, entry k will be NULL.)

- selected_clusts_list:

  A list of lists; entry k of this list is a list of length k of
  clusters (the clusters that were selected by the cluster
  representative lasso). Again, if no set of size k was selected, entry
  k will be NULL.

## References

Reid, S., & Tibshirani, R. (2016). Sparse regression and marginal
testing using cluster prototypes. *Biostatistics*, 17(2), 364–376.
<https://doi.org/10.1093/biostatistics/kxv049>.  
Bühlmann, P., Rütimann, P., van de Geer, S., & Zhang, C. H. (2013).
Correlated variables in regression: Clustering and sparse estimation.
*Journal of Statistical Planning and Inference*, 143(11), 1835–1858.
<https://doi.org/10.1016/j.jspi.2013.05.019>.

## Author

Gregory Faletto, Jacob Bien

# Summarize cluster stability selection output

Produce a concise, computable overview of a fitted `cssr` object: a
header with the number of selected clusters and features (at the given
cutoff), a per-cluster table built on top of
[`printCssDf()`](printCssDf.md), and a placeholder for per-family error
rate (PFER) error-control content.

## Usage

``` r
# S3 method for class 'summary.cssr'
print(x, ...)

# S3 method for class 'cssr'
summary(
  object,
  cutoff = 0,
  min_num_clusts = 1,
  max_num_clusts = NA,
  weighting = "sparse",
  ...
)
```

## Arguments

- x:

  An object of class "summary.cssr" (the output of `summary.cssr()`).

- ...:

  Additional arguments (currently unused).

- object:

  An object of class "cssr" (the output of the function
  [`css()`](css.md)).

- cutoff:

  Numeric; only those clusters with selection proportions equal to at
  least cutoff are summarized. Must be between 0 and 1. Default is 0 (in
  which case either all clusters are summarized, or max_num_clusts are,
  if max_num_clusts is specified).

- min_num_clusts:

  Integer or numeric; the minimum number of clusters to use regardless
  of cutoff. Default is 1. May be set to 0 to allow a pure cutoff-based
  (threshold) selection that can be empty.

- max_num_clusts:

  Integer or numeric; the maximum number of clusters to use regardless
  of cutoff. Default is NA (in which case max_num_clusts is ignored).

- weighting:

  Character; passed to [`getCssSelections()`](getCssSelections.md) to
  determine the selected features and hence the selected-feature count
  in the header. As in [`selected()`](selected.md), it does NOT affect
  which clusters are selected or the per-cluster table. Must be one of
  "sparse", "weighted_avg", or "simple_avg". Default is "sparse".

## Value

An object of class "summary.cssr": a named list with elements

- n_selected_clusts:

  Integer; the number of selected clusters.

- n_selected_feats:

  Integer; the number of selected features.

- cutoff:

  Numeric; the cutoff that was used.

- table:

  A data.frame with one row per selected cluster (the output of
  [`printCssDf()`](printCssDf.md)), or NULL when the selection is empty.

- pfer:

  A placeholder (NA) for per-family error rate error-control content, to
  be filled in by a future release (issue \#87).

## Details

The returned object has class "summary.cssr" and an accompanying print
method (`print.summary.cssr()`). Its `table` element is exactly the
data.frame returned by [`printCssDf()`](printCssDf.md): one row per
selected cluster, sorted by selection proportion, with columns
ClustName, ClustProtoName (only if the features are named),
ClustProtoNum, ClustSelProp (the cluster's selection proportion) and
ClustSize.

If the selection is empty (for example `cutoff = 1` with
`min_num_clusts = 0` when no cluster reaches the cutoff), `summary.cssr`
returns a well-formed zero-row object without error: `table` is NULL and
the header reports "0 clusters / 0 features selected". In this case
[`printCssDf()`](printCssDf.md) and its helper are deliberately NOT
called, as they require at least one selected cluster.

The `pfer` element is a documented placeholder (NA) reserved for
error-control content to be filled in by a future release (issue \#87).

## See also

[`selected()`](selected.md) to extract just the selected clusters or
features; [`getCssSelections()`](getCssSelections.md) for the underlying
selection; [`printCssDf()`](printCssDf.md) for the per-cluster
data.frame; [`print.cssr()`](print.cssr.md) for the analogous printed
summary.

## Author

Gregory Faletto, Jacob Bien

## Examples

``` r
set.seed(1)
data <- genClusteredData(n = 50, p = 11, k_unclustered = 2,
  cluster_size = 4, n_clusters = 1, snr = 3)
clusters <- list(cluster1 = 1:4)
res <- css(X = data$X, y = data$y, lambda = 0.01, clusters = clusters,
  B = 10)
summary(res)
#> Cluster stability selection summary
#> 8 clusters / 8 features selected at cutoff 0
#> 
#>   ClustName ClustProtoNum ClustSelProp ClustSize
#> 1  cluster1             3         1.00         4
#> 2        c2             5         1.00         1
#> 3        c3             6         1.00         1
#> 4        c5             8         1.00         1
#> 5        c6             9         1.00         1
#> 6        c4             7         0.95         1
#> 7        c7            10         0.90         1
#> 8        c8            11         0.90         1
#> 
#> PFER: NA (not yet implemented; see issue #87)
```

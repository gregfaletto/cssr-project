# Prepares a data.frame summarazing cluster stability selection output to print

Print a summary of the information from the css function.

## Usage

``` r
printCssDf(css_results, cutoff = 0, min_num_clusts = 1, max_num_clusts = NA)
```

## Arguments

- css_results:

  An object of class "cssr" (the output of the function css).

- cutoff:

  Numeric; the outputted data.frame will display only those clusters
  with selection proportions equal to at least cutoff. Must be between 0
  and 1. Default is 0 (in which case either all clusters are displayed,
  or max_num_clusts are, if max_num_clusts is specified).

- min_num_clusts:

  Integer or numeric; the minimum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns fewer than
  min_num_clusts clusters, the cutoff will be lowered until at least
  min_num_clusts clusters are selected.) Default is 1.

- max_num_clusts:

  Integer or numeric; the maximum number of clusters to use regardless
  of cutoff. (That is, if the chosen cutoff returns more than
  max_num_clusts clusters, the cutoff will be raised until at most
  max_num_clusts clusters are selected.) Default is NA (in which case
  max_num_clusts is ignored).

## Value

A data.frame; each row contains a cluster, arranged in decreasing order
of cluster selection proportion from top to bottom. The columns are
ClustName (the name of the cluster that was either provided to css or
made by css if no name was provided); ClustProtoName (the name of the
selection prototype from the cluster, which is the feature with the
greatest individual selection proportion among all the cluster members,
with ties broken by choosing the feature with the highest correlation
with the response if the response is real-valued; only returned if the
features are named), ClustProtoNum (the column number of the prototype
in the X matrix provided to css), ClustSelProp (the cluster's selection
proportion), and ClustSize (the size of the cluster).

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
printCssDf(res)
#>   ClustName ClustProtoNum ClustSelProp ClustSize
#> 1  cluster1             3         1.00         4
#> 2        c2             5         1.00         1
#> 3        c3             6         1.00         1
#> 4        c5             8         1.00         1
#> 5        c6             9         1.00         1
#> 6        c4             7         0.95         1
#> 7        c7            10         0.90         1
#> 8        c8            11         0.90         1
```

# Advanced usage

This vignette covers additional features of the package: the competitor
methods compared against in the paper, helpers for choosing a model
size, and the data generators used in the simulation studies. For the
core workflow, see the [getting-started
guide](https://gregfaletto.github.io/cssr-project/) (which also covers
custom base selectors via `fitfun` and choosing `lambda` with
[`getLassoLambda()`](../reference/getLassoLambda.md)) and
[`vignette("prediction", "cssr")`](../articles/prediction.md).

``` r
library(cssr)
set.seed(983219)

data <- genClusteredData(n = 80, p = 40, cluster_size = 10,
                         k_unclustered = 10, snr = 3)
X <- data$X
y <- data$y
clusters <- list("Z_cluster" = 1:10)
```

## Competitor methods

The package ships the two competitor methods that [Faletto and Bien
(2022)](https://arxiv.org/abs/2201.00494) compare against. Both handle
clusters of correlated features, but differently:

- [`protolasso()`](../reference/protolasso.md) ([Reid and Tibshirani,
  2016](https://doi.org/10.1515/sagmb-2017-0027)) picks a single
  *prototype* from each cluster and then runs the lasso.
- [`clusterRepLasso()`](../reference/clusterRepLasso.md) ([Bühlmann et
  al., 2013](https://doi.org/10.1111/j.1467-9868.2012.01072.x)) replaces
  each cluster with its *average* (a cluster representative) and then
  runs the lasso.

``` r
proto <- protolasso(X, y, clusters)
str(proto, max.level = 1)
#> List of 3
#>  $ selected_sets       :List of 31
#>  $ selected_clusts_list:List of 31
#>  $ beta                :Formal class 'dgCMatrix' [package "Matrix"] with 6 slots

crl <- clusterRepLasso(X, y, clusters)
str(crl, max.level = 1)
#> List of 3
#>  $ selected_sets       :List of 31
#>  $ selected_clusts_list:List of 31
#>  $ beta                :Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
```

Both return selected sets along a lasso path. In the paper’s simulations
and data application, cluster stability selection is generally more
stable and predictive than either; see
[`vignette("prediction", "cssr")`](../articles/prediction.md) and the
paper for the comparison.

## Choosing a model size: `getModelSize()`

[`getModelSize()`](../reference/getModelSize.md) suggests a data-driven
model size by fitting the lasso on the full data set. It is a convenient
default for the `max_num_clusts` argument of
[`cssSelect()`](../reference/cssSelect.md) /
[`getCssSelections()`](../reference/getCssSelections.md).

``` r
getModelSize(X, y, clusters)
#> [1] 10
```

## Data generators for simulations

[`genClusteredData()`](../reference/genClusteredData.md) (used above and
in the getting-started guide) is the basic generator. Two richer
variants reproduce the settings in the paper’s simulation studies, where
cluster members are proxies of *differing* quality:

- [`genClusteredDataWeighted()`](../reference/genClusteredDataWeighted.md)
  — cluster members have different (fixed) noise levels;
  `n_strong_cluster_vars` of them are low-noise “strong” proxies.
- [`genClusteredDataWeightedRandom()`](../reference/genClusteredDataWeightedRandom.md)
  — like the above, but the members’ noise levels are drawn at random.

``` r
set.seed(1)
dw <- genClusteredDataWeighted(n = 80, p = 40, k_unclustered = 10,
                               cluster_size = 10, n_strong_cluster_vars = 3,
                               snr = 3)
str(dw, max.level = 1)
#> List of 4
#>  $ X : num [1:80, 1:40] -0.602 0.334 -0.749 1.082 -0.142 ...
#>  $ y : num [1:80] 0.334 0.112 3.954 1.039 2.553 ...
#>  $ Z : num [1:80, 1] -0.626 0.184 -0.836 1.595 0.33 ...
#>  $ mu: num [1:80] -1.311 0.731 2.142 -0.754 1.579 ...

dwr <- genClusteredDataWeightedRandom(n = 80, p = 40, k_unclustered = 10,
                                      cluster_size = 10, snr = 3)
str(dwr, max.level = 1)
#> List of 4
#>  $ X : num [1:80, 1:40] -0.439 1.888 0.423 2.505 -1.799 ...
#>  $ y : num [1:80] -2.258 2.116 0.525 -0.892 -1.288 ...
#>  $ Z : num [1:80, 1] -0.503 1.922 0.566 2.619 -1.399 ...
#>  $ mu: num [1:80] -1.388 1.962 0.756 1.406 -3.157 ...
```

The errors-in-variables model behind these generators treats each
observed proxy as a latent signal plus noise.
[`getNoiseVar()`](../reference/getNoiseVar.md) returns the noise
variance that produces a target correlation between a proxy and the
latent signal:

``` r
# Noise variance giving a proxy-to-latent correlation of 0.9
getNoiseVar(0.9)
#> [1] 0.2345679
```

## See also

- The [getting-started
  guide](https://gregfaletto.github.io/cssr-project/): selecting
  features, custom `fitfun`,
  [`getLassoLambda()`](../reference/getLassoLambda.md).
- [`vignette("prediction", "cssr")`](../articles/prediction.md):
  prediction with cluster representatives.
- The [annotated source
  bookdown](https://gregfaletto.github.io/cssr-project/create/) for the
  full implementation.

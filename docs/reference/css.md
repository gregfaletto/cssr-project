# Cluster Stability Selection

Executes cluster stability selection algorithm. Takes subsamples of
data, executes feature selection algorithm on each subsample, and
returns matrices of feature selection indicators as well as cluster
selection indicators.

## Usage

``` r
css(
  X,
  y,
  lambda,
  clusters = list(),
  fitfun = cssLasso,
  sampling_type = "SS",
  B = ifelse(sampling_type == "MB", 100L, 50L),
  prop_feats_remove = 0,
  train_inds = integer(),
  num_cores = 1L
)
```

## Arguments

- X:

  An n x p numeric matrix (preferably) or a data.frame (which will be
  coerced internally to a matrix by the function model.matrix)
  containing p \>= 2 features/predictors.

- y:

  The response; can be anything that takes the form of an n-dimensional
  vector, with the ith entry corresponding to the ith row of X.
  Typically (and for default `fitfun = cssLasso`), `y` should be an
  n-dimensional numeric vector.

- lambda:

  A tuning parameter or set of tuning parameters that may be used by the
  feature selection method `fitfun`. In the default case when
  `fitfun = cssLasso`, lambda should be a numeric: the penalty to use
  for each lasso fit. (`css()` does not require lambda to be any
  particular object because for a user-specified feature selection
  method `fitfun`, lambda can be an arbitrary object. See the
  description of `fitfun` below.)

- clusters:

  A list of integer vectors; each vector should contain the indices of a
  cluster of features (a subset of `1:p`). (If there is only one
  cluster, clusters can either be a list of length 1 or an integer
  vector.) All of the provided clusters must be non-overlapping. Every
  feature not appearing in any cluster will be assumed to be unclustered
  (that is, they will be treated as if they are in a "cluster"
  containing only themselves). If clusters is a list of length 0 (or a
  list only containing clusters of length 1), then `css()` returns the
  same results as stability selection (so the returned `feat_sel_mat`
  will be identical to `clus_sel_mat`). Names for the clusters will be
  needed later; any clusters that are not given names in the provided
  list will be given names automatically by `css()`. CAUTION: if the
  provided X is a data.frame that contains a categorical feature with
  more than two levels, then the resulting matrix made from model.matrix
  will have a different number of columns than the provided data.frame,
  some of the feature numbers will change, and the clusters argument
  will not work properly (in the current version of the package). To get
  correct results in this case, please use model.matrix to convert the
  data.frame to a numeric matrix on your own, then provide this matrix
  and cluster assignments with respect to this matrix. Default is
  [`list()`](https://rdrr.io/r/base/list.html) (so no clusters are
  specified).

- fitfun:

  A function; the feature selection function used on each subsample by
  cluster stability selection. This can be any feature selection method;
  the only requirement is that it accepts the arguments (and only the
  arguments) `X`, `y`, and `lambda` and returns an integer vector that
  is a subset of `1:p`. For example, `fitfun` could be best subset
  selection or forward stepwise selection or LARS and `lambda` could be
  the desired model size; or `fitfun` could be the elastic net and
  `lambda` could be a length-two vector specifying lambda and alpha.
  Default is `cssLasso`, an implementation of lasso (relying on the R
  package `glmnet`), where `lambda` must be a positive numeric
  specifying the L1 penalty for the `lasso`.

- sampling_type:

  A character vector; either "SS" or "MB". For "MB", all B subsamples
  are drawn randomly (as proposed by Meinshausen and Bühlmann 2010). For
  "SS", in addition to these B subsamples, the B complementary pair
  subsamples will be drawn as well (see Faletto and Bien 2022 or Shah
  and Samworth 2013 for details). Default is "SS", and "MB" is not
  supported yet.

- B:

  Integer or numeric; the number of subsamples. Note: For
  `sampling_type=="MB"` the total number of subsamples will be `B`; for
  `sampling_type="SS"` the number of subsamples will be `2*B`. Default
  is 100 for `sampling_type="MB"` and 50 for `sampling_type="SS"`.

- prop_feats_remove:

  Numeric; if `prop_feats_remove` is greater than 0, then on each
  subsample, each feature is randomly dropped from the design matrix
  that is provided to `fitfun` with probability `prop_feats_remove`
  (independently across features). That is, in a typical subsample,
  `prop_feats_remove*p` features will be dropped (though this number
  will vary). This is similar in spirit (but distinct from) extended
  stability selection (Beinrucker et. al. 2016); see their paper for
  some of the benefits of dropping features (besides increasing
  computational speed and decreasing memory requirements). For
  `sampling_type="SS"`, the features dropped in each complementary pair
  of subsamples are identical in order to ensure that the theoretical
  guarantees of Faletto and Bien (2022) are retained within each
  individual pair of subsamples. (Note that this feature is not
  investigated either theoretically or in simulations by Faletto and
  Bien 2022). Must be between 0 and 1. Default is 0.

- train_inds:

  Optional; an integer or numeric vector containing the indices of
  observations in `X` and `y` to set aside for model training by the
  function [`getCssPreds()`](getCssPreds.md) after feature selection.
  (This will only work if `y` is real-valued, because
  [`getCssPreds()`](getCssPreds.md) using ordinary least squares
  regression to generate predictions.) If `train_inds` is not provided,
  all of the observations in the provided data set will be used for
  feature selection.

- num_cores:

  Optional; an integer. If using parallel processing, the number of
  cores to use for parallel processing (`num_cores` will be supplied
  internally as the `mc.cores` argument of
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)).

## Value

A list containing the following items:

- `feat_sel_mat`:

  A `B` (or `2*B` for `sampling_type="SS"`) x `p` numeric (binary)
  matrix. `feat_sel_mat[i, j] = 1` if feature `j` was selected by the
  base feature selection method on subsample `i`, and 0 otherwise.

- `clus_sel_mat`:

  A `B` (or `2*B` for SS sampling) x `length(clusters)` numeric (binary)
  matrix. `clus_sel_mat[i, j] = 1` if at least one feature from cluster
  j was selected by the base feature selection method on subsample `i`,
  and 0 otherwise.

- `X`:

  The `X` matrix provided to `css()`, coerced from a data.frame to a
  matrix if needed.

- `y`:

  The `y` vector provided to `css()`.

- `clusters`:

  A named list of integer vectors containing all of the clusters
  provided to `css()`, as well as size 1 clusters of any features not
  listed in any of the clusters provided to `css()`. All clusters will
  have names; any clusters not provided with a name in the input to
  `css()` will be given names automatically by `css()` (of the form c1,
  etc.).

- `train_inds`:

  Identical to the `train_inds` provided to `css()`.

## References

Faletto, G., & Bien, J. (2022). Cluster Stability Selection. *arXiv
preprint arXiv:2201.00494*. <https://arxiv.org/abs/2201.00494>.

Shah, R. D., & Samworth, R. J. (2013). Variable selection with error
control: Another look at stability selection. *Journal of the Royal
Statistical Society. Series B: Statistical Methodology*, 75(1), 55–80.
<https://doi.org/10.1109/RITA.2014.2302071>.

Meinshausen, N., & Bühlmann, P. (2010). Stability Selection. *Journal of
the Royal Statistical Society. Series B: Statistical Methodology*,
72(4), 417–473.
<https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1467-9868.2010.00740.x>.

Beinrucker, A., Dogan, Ü., & Blanchard, G. (2016). Extensions of
stability selection using subsamples of observations and covariates.
*Statistics and Computing*, 26(5), 1059- 1077.
<https://doi.org/10.1007/s11222-015-9589-y>.

Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization
Paths for Generalized Linear Models via Coordinate Descent. *Journal of
Statistical Software*, 33(1), 1-22. URL
<https://www.jstatsoft.org/v33/i01/>.

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
print(res)
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

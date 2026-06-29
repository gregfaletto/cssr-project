# Prediction with cluster representatives

The getting-started guide (the package [home
page](https://gregfaletto.github.io/cssr-project/)) shows how to
*select* features and clusters with cluster stability selection. This
vignette shows how to use a fitted model to **predict** the response for
new observations.

Cluster stability selection predicts by forming **cluster
representatives** — weighted averages of the members of each selected
cluster — and fitting ordinary least squares on those representatives.
Because each cluster member is a noisy proxy for the same underlying
signal, averaging the members estimates that signal better than any
single member would, which tends to improve prediction. (See [Faletto
and Bien, 2022](https://arxiv.org/abs/2201.00494).)

``` r
library(cssr)
set.seed(983219)

# Data with a cluster of 10 correlated proxies (features 1-10), split into a
# training set and a test set.
data <- genClusteredData(n = 120, p = 40, cluster_size = 10,
                         k_unclustered = 10, snr = 3)

X_train <- data$X[1:80, ]
y_train <- data$y[1:80]
X_test  <- data$X[81:120, ]
y_test  <- data$y[81:120]

clusters <- list("Z_cluster" = 1:10)
```

## The quick way: `cssPredict()`

[`cssPredict()`](../reference/cssPredict.md) runs the whole pipeline —
selection, cluster-representative formation, the downstream regression,
and prediction — in a single call. It is the easiest way to get
predictions.

``` r
preds <- cssPredict(X_train, y_train, X_test, clusters)
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold
head(preds)
#> [1]  2.7118284 -0.6048872  2.5147773 -1.0428944  1.2369631 -2.5685820

# Test-set root mean squared error
sqrt(mean((preds - y_test)^2))
#> [1] 1.603597
```

## More control: `css()` + `getCssPreds()`

As in the getting-started guide, [`css()`](../reference/css.md) does the
expensive stability-selection step once; you can then reuse its output.
To predict, [`getCssPreds()`](../reference/getCssPreds.md) forms the
cluster representatives and fits the downstream ordinary-least-squares
model.

That regression needs its own training data, kept separate from the data
used for selection. The simplest way is to reserve some rows with
[`css()`](../reference/css.md)’s `train_inds` argument — those rows are
held out of the selection step and used only to fit the downstream
model.

``` r
lambda <- getLassoLambda(X_train, y_train)

# Reserve rows 1-40 to fit the downstream model; the rest drive selection.
css_output <- css(X_train, y_train, lambda, clusters = clusters,
                  train_inds = 1:40)

preds_full <- getCssPreds(css_output, testX = X_test,
                          weighting = "weighted_avg", cutoff = 0.3)
head(preds_full)
#> [1]  2.4153868 -0.1685438  2.9107732 -2.0774436  1.7627208 -1.6097944
```

Alternatively, if you ran [`css()`](../reference/css.md) without
`train_inds`, supply the downstream training data directly through
`trainX`/`trainY`:

``` r
css_no_train <- css(X_train, y_train, lambda, clusters = clusters)

preds_ext <- getCssPreds(css_no_train, testX = X_test, trainX = X_train,
                         trainY = y_train, weighting = "weighted_avg",
                         cutoff = 0.3)
head(preds_ext)
#> [1]  2.8360818 -0.7199053  3.4580413 -2.5822325  1.8442163 -2.2824792
```

## Weighting schemes

The `weighting` argument controls how cluster representatives are
formed:

- `"simple_avg"` — a plain (equally weighted) average of the cluster
  members. Optimal when the proxies are equally noisy.
- `"weighted_avg"` — a weighted average with weights inferred from the
  selection proportions, so noisier proxies get less weight. Best when
  the members’ noise levels vary.
- `"sparse"` — use a single member (the cluster prototype), like the
  protolasso. Simplest, but discards the other members.

``` r
for (w in c("simple_avg", "weighted_avg", "sparse")) {
  p <- getCssPreds(css_output, testX = X_test, weighting = w, cutoff = 0.3)
  cat(sprintf("%-12s test RMSE: %.3f\n", w, sqrt(mean((p - y_test)^2))))
}
#> simple_avg   test RMSE: 1.539
#> weighted_avg test RMSE: 1.561
#> sparse       test RMSE: 1.785
```

## Just the design matrix: `getCssDesign()`

If you would rather build your own downstream model (anything other than
OLS), [`getCssDesign()`](../reference/getCssDesign.md) returns the
matrix of cluster representatives for new data, which you can pass to
any modeling function.

``` r
X_test_reps <- getCssDesign(css_output, newX = X_test,
                            weighting = "weighted_avg", cutoff = 0.3)

# One column per selected cluster
dim(X_test_reps)
#> [1] 40  8
head(X_test_reps)
#>       Z_cluster         c2         c3         c5         c6         c8
#> [1,]  0.4276269  0.6104695  1.3396722  0.3624032 -0.5740362  0.7301855
#> [2,] -0.1654293 -0.7046012 -0.6660738  0.1276078 -0.8733820  1.6915040
#> [3,]  1.5239779  1.3879518 -0.8501652  1.2689652  1.4158769 -1.2129715
#> [4,] -1.7916022 -1.9100213  1.9602672 -0.5493360 -1.3984021  1.0010257
#> [5,]  0.8282105  0.4045906 -0.5670002 -0.5113342  1.9551502 -0.5279536
#> [6,] -0.3572601  0.4244883 -0.5436356 -0.9656674 -1.8448312 -0.6148463
#>             c12        c29
#> [1,]  0.2471666  0.9746860
#> [2,] -0.1115737  0.6018539
#> [3,] -1.3925460 -1.4545796
#> [4,]  1.3944063 -0.1857232
#> [5,]  1.4951836 -0.5468257
#> [6,] -1.2928336  0.3328180
```

## See also

- The [getting-started
  guide](https://gregfaletto.github.io/cssr-project/) for selecting
  features and clusters.
- [`vignette("advanced-usage", "cssr")`](../articles/advanced-usage.md)
  for competitor methods, the data generators, and other helpers.

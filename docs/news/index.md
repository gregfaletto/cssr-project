# Changelog

## cssr 0.4.7.9000 (development version)

- [`cssLasso()`](https://gregfaletto.github.io/cssr-project/reference/cssLasso.md)
  now fits its second lasso path over a shortened, anchored penalty grid
  rather than refitting the whole path, which makes
  [`css()`](https://gregfaletto.github.io/cssr-project/reference/css.md)
  measurably faster while returning the same selected features as the
  refit it replaced
  ([\#125](https://github.com/gregfaletto/cssr-project/issues125)).

- [`cssLasso()`](https://gregfaletto.github.io/cssr-project/reference/cssLasso.md)
  now reads its coefficients at the penalty `glmnet` reported rather
  than at the one it was asked for, so a feature whose coefficient is
  floating-point debris from interpolating between two lasso-path
  columns is no longer selected
  ([\#199](https://github.com/gregfaletto/cssr-project/issues199)). This
  changes which features
  [`cssLasso()`](https://gregfaletto.github.io/cssr-project/reference/cssLasso.md)
  and
  [`css()`](https://gregfaletto.github.io/cssr-project/reference/css.md)
  return – superseding the identity above, which held against the refit
  as it then behaved – where the penalty sits near the top of the lasso
  path fitted to a single subsample, in practice on small, weak-signal
  data.

This file starts here rather than covering the package’s whole history;
for changes before this point, see the closed pull requests and the
commit log.

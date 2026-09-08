# Changelog

## cssr 0.4.7.9000 (development version)

- [`cssLasso()`](https://gregfaletto.github.io/cssr-project/reference/cssLasso.md)
  now fits its second lasso path over a shortened, anchored penalty grid
  rather than refitting the whole path, which makes
  [`css()`](https://gregfaletto.github.io/cssr-project/reference/css.md)
  measurably faster while returning exactly the same selected features
  ([\#125](https://github.com/gregfaletto/cssr-project/issues125)).

- [`cssLasso()`](https://gregfaletto.github.io/cssr-project/reference/cssLasso.md)
  now reads its coefficients at the penalty `glmnet` reported rather
  than at the one it was asked for. The two can differ in their last
  bits, because `glmnet` returns a supplied penalty after dividing it by
  the response scale and multiplying it back; where they did, the
  coefficients read back were a blend of two adjacent columns of the
  lasso path rather than one solved column, and a feature whose
  coefficient in that solved column is zero could be selected on a value
  around `1e-17`. Such features are no longer selected, so this is a
  change in the features
  [`cssLasso()`](https://gregfaletto.github.io/cssr-project/reference/cssLasso.md)
  and
  [`css()`](https://gregfaletto.github.io/cssr-project/reference/css.md)
  return and not a further instance of the identity stated above, which
  is about the grid the second fit runs over. It bites where the penalty
  sits near the top of the lasso path fitted to a single subsample,
  which in practice means small, weak-signal data; at ordinary sizes
  nothing changes
  ([\#199](https://github.com/gregfaletto/cssr-project/issues199)).

This file starts here rather than covering the package’s whole history;
for changes before this point, see the closed pull requests and the
commit log.

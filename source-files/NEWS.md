# cssr 0.4.7.9000 (development version)

* `cssLasso()` now fits its second lasso path over a shortened, anchored penalty
  grid rather than refitting the whole path, which makes `css()` measurably
  faster while returning the same selected features as the refit it replaced
  (#125).

* `cssLasso()` now reads its coefficients at the penalty `glmnet` reported
  rather than at the one it was asked for, so a feature whose coefficient is
  floating-point debris from interpolating between two lasso-path columns is no
  longer selected (#199). This supersedes the identity above, which held against
  the refit as it then behaved. It is uncommon, and most likely where the penalty
  sits near the top of the lasso path fitted to a single subsample.

  **At the `cssLasso()` level the change can only remove a feature.** The
  interpolated coefficient vector's support is a superset of the solved column's,
  because a feature carried only by the adjacent column leaks in at around
  `1e-17` and is selected on `abs(x) > 0`.

  **At the `css()` level it can go either way**, and callers of `cssSelect()` and
  `cssPredict()` should know it. A removal upstream lowers a cluster's selection
  proportion, which can reach a `max_num_clusts` tie; `getSelectedClusters()`
  resolves such a tie by returning *more* clusters and warning. So a returned
  cluster set can widen, the `Returning more than max_num_clusts` warning can
  fire where it did not before, and under `options(warn = 2)` a call that
  returned a result can now stop with an error.

This file starts here rather than covering the package's whole history; for
changes before this point, see the closed pull requests and the commit log.

# cssr 0.4.7.9000 (development version)

* `cssLasso()` now fits its second lasso path over a shortened, anchored penalty
  grid rather than refitting the whole path, which makes `css()` measurably
  faster while returning the same selected features as the refit it replaced
  (#125).

* `cssLasso()` now reads its coefficients at the penalty `glmnet` reported
  rather than at the one it was asked for, so a feature whose coefficient is
  floating-point debris from interpolating between two lasso-path columns is no
  longer selected (#199). This changes which features `cssLasso()` and `css()`
  return -- superseding the identity above, which held against the refit as it
  then behaved. It is rare, and every change observed was a feature no longer
  being selected. It can arise anywhere on the lasso path, more often the nearer
  the penalty sits to the top of the path fitted to a single subsample, so it is
  most visible on small or weak-signal data.

This file starts here rather than covering the package's whole history; for
changes before this point, see the closed pull requests and the commit log.

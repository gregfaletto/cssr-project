# cssr 0.4.7.9000 (development version)

* `cssLasso()` now fits its second lasso path over a shortened, anchored penalty
  grid rather than refitting the whole path, which makes `css()` measurably
  faster while returning exactly the same selected features (#125).

This file starts here rather than covering the package's whole history; for
changes before this point, see the closed pull requests and the commit log.

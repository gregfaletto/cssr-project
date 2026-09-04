# Changelog

## cssr 0.4.7.9000 (development version)

- [`cssLasso()`](https://gregfaletto.github.io/cssr-project/reference/cssLasso.md)
  now fits its second lasso path over a shortened, anchored penalty grid
  rather than refitting the whole path, which makes
  [`css()`](https://gregfaletto.github.io/cssr-project/reference/css.md)
  measurably faster while returning exactly the same selected features
  ([\#125](https://github.com/gregfaletto/cssr-project/issues125)).

This file starts here rather than covering the package’s whole history;
for changes before this point, see the closed pull requests and the
commit log.

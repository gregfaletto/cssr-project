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

  **At the `cssLasso()` level the change removes features and does not add
  them.** Two things have to hold: a feature carried only by the adjacent column
  leaks into the blend at around `1e-17` and is selected on `abs(x) > 0`, so the
  blend's support is a superset of the solved column's; and a feature the solved
  column carries survives the blend, since its contribution is scaled by a weight
  indistinguishable from 1, or from 0, at double precision. The second can fail only by exact cancellation
  between two opposite-signed contributions. No such case was found in 239,004
  runs, and no sign flip in 541,750 adjacent-column pairs, so this is a
  well-tested empirical claim rather than a proof.

  **At the `css()` level the change can go either way**, and callers of
  `cssSelect()` and `cssPredict()` should know it, because the aggregation steps
  above `cssLasso()` are not order-preserving. A removal lowers a cluster's
  selection proportion, and a lowered proportion can reach a tie that is resolved
  by returning *more*. Two distinct routes, only one of which announces itself:

  - `getSelectedClusters()` resolving a `max_num_clusts` tie returns more
    clusters **and warns**, so `selected_clusts` can widen, the
    `Returning more than max_num_clusts` warning can fire where it did not
    before, and under `options(warn = 2)` a call that returned can now error.
  - A within-cluster tie in the prototype weighting widens `selected_feats`
    **with no warning at all**, and with `selected_clusts` unchanged. This is the
    route to know about if you rely on warnings to notice a change, since there
    is nothing to catch.

* Corrected the `weighting` documentation on `getCssSelections()`,
  `getCssDesign()` and `getCssPreds()`: under `"sparse"` weighting a tie divides
  the weight equally among the tied members of a cluster, not among the tied
  clusters, as the text had said. `cssSelect()` and `cssPredict()` now also state
  which weighting each of them uses and what that means for what they return, and
  `getCssDesign()` and `getCssPreds()` now carry the exception clause
  `getCssSelections()` already published, for the case where no member of a
  selected cluster was selected on any subsample. Documentation only; no behavior
  changed (#203).

This file starts here rather than covering the package's whole history; for
changes before this point, see the closed pull requests and the commit log.

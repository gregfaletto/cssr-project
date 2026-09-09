# Generated from _main.Rmd: do not edit by hand

#' Recover the fitted penalty a requested one round-tripped into
#'
#' Returns the element of `lambda_fitted` within `n_ulp` units in the last place
#' of `s` when there is one, and `s` itself otherwise. `cssLasso()` predicts at
#' the returned value rather than at `s`, which is what makes
#' `predict.glmnet()` hand back a solved lasso path column, exact zeros
#' included, instead of a blend of two adjacent columns. The comments in the
#' body say why a tolerance this small is enough, why its two ends are bounded
#' by different mechanisms, and what the function deliberately does not check.
#'
#' @param lambda_fitted A numeric vector of penalties (in practice the
#' `$lambda` component of a fitted `glmnet` object). It may contain `NaN`, and
#' it may be empty or entirely `NaN`; the last two return `s` unchanged.
#' @param s The target penalty; a single nonnegative number.
#' @param n_ulp The tolerance, in units in the last place of `s`: an entry of
#' `lambda_fitted` matches `s` when it is within
#' `n_ulp * .Machine$double.eps * s` of it. The scaling by `s` is part of the
#' contract rather than an implementation detail, because the error being
#' tolerated is relative.
#' @return A single number: the matching element of `lambda_fitted` if there is
#' one, and otherwise `s`.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
snapLambdaToGrid <- function(lambda_fitted, s, n_ulp=4L){
    # The one thing this function does check, and it is about s rather than
    # lambda_fitted, so it does not touch the no-validation decision recorded
    # below. WHAT IT ACTUALLY BUYS, measured rather than reasoned, because an
    # earlier revision of this comment claimed the wrong crossing: the sibling
    # anchoredLambdaGrid(lambda_path, s, n_interior) has the same arity and the
    # same second formal and sits in the chunk above, so a crossed call is easy
    # to write. Handing THIS function the anchored site's arguments passes a
    # scalar s, so this line never fires on that crossing, which is caught by the
    # two #199 red-green assertions rather than by this. (Do not restate that as
    # "the suite output is byte-identical with and without this line": since the
    # invariants block gained an expect_error() for this guard, deleting the
    # guard moves the suite whatever else is wrong.) What this line converts is
    # the TRANSPOSED call, where s is the fitted path. Without it the error comes
    # from the && in the tolerance test below -- which.min() itself is untroubled
    # and returns an index -- and reads as a coercion complaint about a length
    # nothing in this file explains. With it the failure names the precondition
    # and the line that holds it. No
    # legitimate caller can reach it -- checkCssLassoInputs() rejects any lambda
    # whose length is not 1 or 2, and the length-2 form is unpacked before the
    # call site -- so it is a tripwire, not a runtime check.
    stopifnot(length(s) == 1L)

    # WHY A TOLERANCE OF A FEW ULP IS THE RIGHT SIZE, and why that is a claim
    # about IEEE-754 rather than about this machine. glmnet() passes a supplied
    # penalty grid through as ulam = as.double(rev(sort(lambda))) and reports
    # $lambda back divided by the response scale and multiplied by it again.
    # That is two correctly-rounded scalar operations -- no matrix arithmetic,
    # so no BLAS -- and it is the whole reason a supplied penalty need not come
    # back verbatim. Predicting at the returned entry rather than at s is what
    # makes glmnet:::lambda.interp() report left == right with frac == 1, so
    # predict.glmnet() returns beta[, left] * 1 + beta[, right] * 0 -- the
    # solved column, exact zeros included -- instead of a blend that leaks the
    # neighbouring column's coefficients in at the scale of 1 - frac. Those
    # leaked coefficients are around 1e-17 and are SELECTED, because
    # glmnet:::nonzeroCoef()'s membership test is abs(x) > 0 with no tolerance
    # (#199).
    #
    # THE INSENSITIVITY OF n_ulp IS ONE-SIDED, and each end is bounded by a
    # different mechanism. Downwards it is not insensitive at all: measured, the
    # suite reddens at n_ulp = 2 and the invariants fixtures stop snapping at 1,
    # so the value is pinned from below and 4 carries a factor of two over the
    # nearest failing one. Upwards, on cssLasso()'s ordinary route,
    # anchoredLambdaGrid() has already placed s in the grid handed to glmnet(),
    # so which.min() below picks s's own echo and no n_ulp, however large,
    # reaches past it; only on a route where s is absent from the fitted path
    # does the spacing of that path become the operative bound -- and on
    # glmnet's default grids that spacing is of order 1e14 ulp, so it is not a
    # bound n_ulp can approach.
    #
    # NOTHING HERE VALIDATES lambda_fitted, and that is a decision rather than
    # an omission -- the same one anchoredLambdaGrid()'s roxygen states for its
    # own requirement on the caller. What is MEASURED, and nothing wider than
    # that, because two earlier revisions of this comment stated a rule and were
    # wrong both times: all(lambda_fitted > 0) reddens the suite, and
    # all(is.finite(lambda_fitted)) reddens it too, because the fixtures
    # deliberately drive a zero-bearing grid and a NaN-bearing one through this
    # function. A type check does NOT -- stopifnot(is.numeric(lambda_fitted))
    # leaves the suite byte-identically green -- so this is not a rule about
    # validation in general and must not be written as one. The reason
    # it matters beyond bookkeeping is the degenerate lasso path: a validating
    # helper would turn that case from an empty selection into an error, on
    # exactly the shape issue #157 was filed to stop crashing, which is what
    # test_that("cssLasso survives a degenerate lasso path (#125)") exists to
    # hold. (Two earlier revisions of this comment enumerated which check
    # reddens which block and were wrong both times; the positive form above is
    # the correction.)
    # The length(j) == 1L clause below is control flow and not validation: an
    # all-NaN or empty lambda_fitted makes which.min() return integer(0), and
    # if(logical(0)) aborts, so the clause turns that case into "return s".
    #
    # THE INVARIANT cssLasso()'S NEW READ ASSUMES, named here rather than
    # asserted, for the reason just given. cssLasso() asserts the class of its
    # FIRST fit and nothing about its second, and until #199 the second fit's
    # $lambda was consumed only by predict.glmnet(), which owns its own
    # contract. What is now silently assumed of it is that it is a numeric
    # vector with at least one non-NaN entry, on the same scale as lambda.
    #
    # THE FIRST FORMAL IS lambda_fitted AND NOT lambda_path DELIBERATELY. The
    # sibling anchoredLambdaGrid(lambda_path, s, n_interior) has the same arity,
    # the same second formal and the chunk immediately above, so a CROSSED call
    # -- this function given the anchored site's arguments, both in their usual
    # order -- type-checks and runs, and would predict at a penalty chosen from
    # the wrong fit's path. (The TRANSPOSED call, arguments swapped, errors on
    # the scalar guard above; and neither is the route to #190's per-s-slot
    # union, which comes from calling predict.glmnet() with no s at all.) The
    # differing name is the one
    # signal at a call site that these take different things: the FIRST fit's
    # path going in, the SECOND fit's returned penalties coming back.
    #
    # The block that enforces this arithmetic is
    # test_that("snapLambdaToGrid holds its invariants (#199)"); the block that
    # enforces the call site in cssLasso() is
    # test_that("cssLasso predicts at the penalty glmnet fitted (#199)").
    j <- which.min(abs(lambda_fitted - s))
    if(length(j) == 1L &&
        abs(lambda_fitted[j] - s) <= n_ulp*.Machine$double.eps*s){
        return(lambda_fitted[j])
    }
    return(s)
}

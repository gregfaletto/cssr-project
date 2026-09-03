# Generated from _main.Rmd: do not edit by hand

#' Build an anchored, padded lambda grid for a second glmnet fit
#'
#' Returns a shortened version of the augmented penalty grid
#' `unique(rev(sort(c(s, lambda_path))))` that `predict.glmnet(exact = TRUE)`
#' refits over: that grid's prefix through one element past `s`, a few
#' geometrically spaced interior points, and that grid's own final element
#' re-appended. `cssLasso()` fits over this instead of over the whole augmented
#' grid. The comments in the body say why the first and last elements are
#' carried over unchanged, why each guard is there, and which test block
#' enforces the construction.
#'
#' @param lambda_path A numeric vector of penalties (in practice the `$lambda`
#' component of a fitted `glmnet` object). This is a requirement on the caller
#' rather than a claim about what `glmnet()` returns: because the interior
#' points are spaced in log space, `lambda_path` must not contain a zero unless
#' `s` is at or below its smallest positive entry. Any other path containing a
#' zero takes `log(0)` and errors in `seq()`; nothing checks it at runtime.
#' @param s The target penalty; a single nonnegative number.
#' @param n_interior The number of geometrically spaced interior points padding
#' the gap below `s`. This is a speed constant and not a correctness one: the
#' coefficients the returned grid yields at `s` do not depend on it, and 5 was
#' chosen by end-to-end timing of `css()`, in particular for the n < p corner
#' where 0 interior points is a net loss. Changing it changes speed only.
#' @return A strictly decreasing numeric vector with no duplicated entries,
#' whose first and last elements are `identical()` to those of
#' `unique(rev(sort(c(s, lambda_path))))`.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
anchoredLambdaGrid <- function(lambda_path, s, n_interior=5L){
    # g_full is the grid predict.glmnet(exact=TRUE) refits over -- the fitted
    # path augmented with s, de-duplicated and ordered decreasing. The
    # expression is glmnet's own operator for operator, so both sides drop NA and
    # NaN (sort() keeps +-Inf) and collapse repeats identically.
    g_full <- unique(rev(sort(c(s, lambda_path))))
    k_full <- length(g_full)

    # glmnet walks a grid in decreasing order with each solution warm-starting
    # the next, so the solution at s depends only on penalties at or above s;
    # kt counts those. The prefix is kept through one element PAST s because
    # predict.glmnet interpolates between s's two neighbours in the grid, and
    # that extra element is the one lambda.interp() calls `right` -- the next
    # penalty DOWN, since the grid is ordered decreasing. The min() caps the index
    # for the case where kt already reaches the end of g_full and there is no
    # further element to keep -- s = 0, and any s at or below min(lambda_path).
    kt <- sum(g_full >= s)
    keep <- g_full[1:min(k_full, kt + 1L)]

    lo <- g_full[k_full]
    hi <- keep[length(keep)]

    # Two cases have no gap to pad: n_interior <= 0, the caller asking for no
    # padding, and hi <= lo, keep already reaching the end of g_full. That is a
    # wider set than the min() cap above: it holds whenever s is the smallest or
    # the SECOND smallest element of g_full, so a user penalty falling between
    # the two smallest fitted ones also lands here and gets the augmented grid
    # unshortened, which is correct and saves nothing. This guard is also what
    # keeps log(0) out of the seq() below on a degenerate glmnet path, which is
    # a case that genuinely occurs rather than a hypothetical one. The condition
    # is max(abs(crossprod(X, y))) == 0 EXACTLY -- not merely an orthogonal
    # design, and not a numerically small value: measured, an orthonormal X with
    # a generic y gives an ordinary path, and so does one with max|X'y| at 1e-16.
    # At exact zero glmnet returns NaN followed by zeros for $lambda; sort() then
    # drops the NaN and unique() collapses the zeros, leaving a g_full of two
    # elements that lands here -- verified by handing such a design to this
    # function directly. NOTE what is NOT true: no test in this suite exercises
    # this branch with a degenerate path. The #157 fixture builds one (paired
    # +1/-1 integer rows -- grep 'lasso path selects nothing (#157)') but reaches
    # glmnet through clusterLassoCore(), which never calls cssLasso(), so it
    # makes no call to this function at all. Measured across the whole suite:
    # every call here arrives with an ordinary path.
    if(n_interior <= 0 | hi <= lo){
        return(unique(c(keep, lo)))
    }

    # Geometrically spaced filler between the two anchors, with the anchors
    # themselves dropped back off...
    mid <- exp(seq(log(hi), log(lo), length.out=n_interior + 2L))
    mid <- mid[-c(1L, length(mid))]
    # ...and then any filler point that floating-point rounding placed on top of
    # an anchor removed. This filter is load-bearing rather than belt-and-
    # braces: without it the returned grid can repeat an anchor, which is
    # visible only on a path whose entries are a few ULPs apart.
    mid <- mid[mid < hi & mid > lo]

    # lo is re-appended rather than simply left at the end of a longer prefix,
    # and this is the load-bearing line. glmnet:::lambda.interp(), which
    # predict.glmnet() always calls, normalises by lambda[1] - lambda[k].
    #
    # READ THIS BEFORE DELETING THE ANCHOR ON ALGEBRAIC GROUNDS. That normaliser
    # CANCELS in exact arithmetic: work the two ratios through and the weight is
    # just (lambda_right - s)/(lambda_right - lambda_left), with no dependence on
    # the grid's last element at all. So a maintainer who reasons this through
    # symbolically will correctly conclude the anchor is unnecessary -- and be
    # wrong, because in floating point the cancellation is inexact. Measured over
    # 1000 s values on two grids sharing every element but the last: the weight
    # differed in 74% of them, by up to 6 ULP, with left and right identical.
    # The anchor exists to reproduce the ROUNDING, not the algebra. Handing over
    # the same first and last elements the exact refit would have handed over is
    # what makes the interpolated coefficients agree bit for bit.
    #
    # This construction was validated over a six-figure sweep of designs, alphas
    # and s placements; the evidence is in PR #200, which is where to look
    # rather than at any local planning directory. Do not "simplify" it without
    # re-running that evidence.
    # The block that enforces it is test_that("anchoredLambdaGrid holds its
    # invariants (#125)"), NOT test_that("cssLasso is byte-identical to the
    # exact refit (#125)"): every natural simplification of this construction --
    # dropping this re-appended anchor, keeping the prefix through kt rather than
    # kt + 1, and dropping the filter above -- leaves that identity pin entirely
    # green.
    return(c(keep, sort(unique(mid), decreasing=TRUE), lo))
}

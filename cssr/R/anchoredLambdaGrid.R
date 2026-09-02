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
#' points are spaced in log space, `lambda_path` must not contain a zero below
#' its second-smallest positive entry. Such a path takes `log(0)` and errors in
#' `seq()`; nothing checks it at runtime.
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
    # expression is character-for-character glmnet's own, so both sides discard
    # non-finite path entries and collapse repeats identically.
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
    # padding, and hi <= lo, keep already reaching the end of g_full (again
    # s = 0 and any s at or below min(lambda_path)). This guard is also what
    # keeps log(0) out of the seq() below on a degenerate glmnet path, which is
    # a case that genuinely occurs rather than a hypothetical one: on an exactly
    # orthogonal design glmnet returns NaN followed by zeros for $lambda, and
    # sort() drops the NaN while unique() collapses the zeros, leaving a g_full
    # of two elements that lands here.
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
    # predict.glmnet() always calls, normalises by lambda[1] - lambda[k], so the
    # blend weight at s depends on the last element of the supplied grid as much
    # as on the first. Handing over the same first and last elements the exact
    # refit would have handed over is what makes the interpolated coefficients
    # agree bit for bit.
    #
    # This construction was validated over a six-figure sweep of designs, alphas
    # and s placements; do not "simplify" it without re-running that evidence.
    # The block that enforces it is test_that("anchoredLambdaGrid holds its
    # invariants (#125)"), NOT test_that("cssLasso is byte-identical to the
    # exact refit (#125)"): both of the natural simplifications -- dropping this
    # re-appended anchor, and keeping the prefix through kt rather than kt + 1
    # -- leave that identity pin entirely green.
    return(c(keep, sort(unique(mid), decreasing=TRUE), lo))
}

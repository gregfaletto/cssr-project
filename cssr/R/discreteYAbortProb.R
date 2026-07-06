# Generated from _main.Rmd: do not edit by hand

#' Estimated probability the default cssLasso aborts on a constant-y subsample
#'
#' For the default `cssLasso`, `css()` draws `n_sub` random subsamples of the
#' selection set, each a size-`floor(n/2)` draw without replacement from the
#' `n = length(y)` responses; `cssLasso` aborts on any subsample whose `y` has a
#' single unique value (see `checkCssLassoInputs()`). This returns the estimated
#' probability that at least one of those subsamples is all-constant. Per
#' subsample, P(all one value) is hypergeometric, `sum_v C(count_v, m) / C(n, m)`
#' with `m = floor(n/2)` (a term is 0 unless some value's count is at least `m`,
#' i.e. that value is a near-majority), computed in log space (`lchoose`) to
#' avoid overflow for large `n`. That per-subsample probability is aggregated
#' over the `n_sub` subsamples treating them as independent (a close
#' approximation; the small SS within-pair dependence is negligible near the
#' threshold).
#'
#' @param y A numeric or integer response vector (the selection-set response).
#' @param B Integer; the number of subsamples (`2*B` are drawn when
#' `sampling_type = "SS"`).
#' @param sampling_type A character string; "SS" (the reachable default) draws
#' `2*B` subsamples, anything else draws `B`.
#' @return A single number in `[0, 1]`: the estimated probability that at least
#' one size-`floor(n/2)` subsample of `y` is all-constant (and so aborts the
#' default `cssLasso`).
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
discreteYAbortProb <- function(y, B, sampling_type){
    n <- length(y)
    m <- floor(n / 2)
    n_sub <- if(sampling_type == "SS") 2L * B else as.integer(B)
    counts <- as.integer(table(y))
    p_const <- sum(exp(lchoose(counts, m) - lchoose(n, m)))  # sum_v C(count_v,m)/C(n,m)
    p_const <- min(max(p_const, 0), 1)                       # guard fp drift to [0,1]
    1 - (1 - p_const)^n_sub
}

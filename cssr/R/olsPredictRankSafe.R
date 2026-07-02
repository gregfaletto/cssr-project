# Generated from _main.Rmd: do not edit by hand

#' Predict from a possibly rank-deficient OLS fit without crashing on R >= 4.4
#'
#' On R >= 4.4 `stats::predict.lm()`'s default `rankdeficient = "warnif"` runs
#' `qr.Q(qr.default(t(qr.R(model$qr))))`, which aborts with "NA/NaN/Inf in
#' foreign function call" when `qr.R(model$qr)` contains NaN/Inf (a rank-deficient
#' fit plus a LINPACK numerical pathology seen on real high-dimensional data).
#' `rankdeficient = "simple"` restores the pre-4.4 behavior: predict from the
#' estimable coefficients, yielding finite predictions. That argument only exists
#' on R >= 4.4, so it is added conditionally (a plain retry with the argument
#' would itself error on older R with "unused argument"). The `"simple"` path
#' emits the stock "rank-deficient fit" warning; muffle only that specific
#' warning (its advice to use `rankdeficient = "NA"` would NA-fill non-estimable
#' cases and break the finiteness contract in `getCssPreds()`) so unrelated
#' warnings still surface. On full-rank fits this is bit-identical to the old
#' default.
#' @noRd
olsPredictRankSafe <- function(model, newdata) {
    pred_args <- list(object = model, newdata = newdata)
    if (getRversion() >= "4.4.0") {
        pred_args$rankdeficient <- "simple"
    }
    withCallingHandlers(
        do.call(stats::predict.lm, pred_args),
        warning = function(w) {
            if (grepl("rank-deficient fit", conditionMessage(w), fixed = TRUE)) {
                invokeRestart("muffleWarning")
            }
        }
    )
}

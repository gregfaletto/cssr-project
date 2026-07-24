# Generated from _main.Rmd: do not edit by hand

#' Is the current session running on Windows?
#'
#' parallel::mclapply relies on forking, which is unavailable on Windows; css()
#' uses this to downgrade num_cores > 1 to serial there rather than let mclapply
#' hard-error (#155d). Factored into a helper so the Windows branch is
#' unit-testable off-Windows via local_mocked_bindings.
#' @return TRUE if running on Windows, FALSE otherwise.
#' @author Gregory Faletto, Jacob Bien
#' @keywords internal
#' @noRd
runningOnWindows <- function(){
    identical(.Platform$OS.type, "windows")
}

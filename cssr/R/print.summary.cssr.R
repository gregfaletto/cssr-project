# Generated from _main.Rmd: do not edit by hand

#' @param x An object of class "summary.cssr" (the output of [summary.cssr()]).
#' @rdname summary.cssr
#' @export
print.summary.cssr <- function(x, ...){
    cat("Cluster stability selection summary\n")
    cat(sprintf("%d cluster%s / %d feature%s selected at cutoff %s\n",
        x$n_selected_clusts, if(x$n_selected_clusts == 1) "" else "s",
        x$n_selected_feats, if(x$n_selected_feats == 1) "" else "s",
        format(x$cutoff)))
    if(is.null(x$table)){
        cat("(no clusters selected at this cutoff)\n")
    } else{
        cat("\n")
        print.data.frame(x$table, ...)
    }
    # PFER placeholder: error-control content to be filled in by issue #87.
    cat(sprintf("\nPFER: %s (not yet implemented; see issue #87)\n",
        format(x$pfer)))
    invisible(x)
}

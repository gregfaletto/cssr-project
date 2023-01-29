# Generated from _main.Rmd: do not edit by hand

#' Helper function to check operations within getSelectedClusters function
#'
#' @param n_sel_clusts The number of selected clusters; should be constrained
#' by min_num_clusts and max_num_clusts (though it may not be possible to
#' satisfy both constraints simulteneously, in which case a warning will be
#' thrown).
#' @param min_num_clusts Integer or numeric; the minimum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns fewer than
#' min_num_clusts clusters, the cutoff will be increased until at least
#' min_num_clusts clusters are selected.)
#' @param max_num_clusts Integer or numeric; the maximum number of clusters to
#' use regardless of cutoff. (That is, if the chosen cutoff returns more than
#' max_num_clusts clusters, the cutoff will be decreased until at most
#' max_num_clusts clusters are selected.) If NA, max_num_clusts is ignored.
#' @param max_sel_prop Numeric; the maximum selection proportion observed for 
#' any cluster.
#' @author Gregory Faletto, Jacob Bien
checkSelectedClusters <- function(n_sel_clusts, min_num_clusts, max_num_clusts,
    max_sel_prop){
    if(n_sel_clusts == 0){
        err <- paste("No clusters selected with this cutoff (try a cutoff below the maximum cluster selection proportion, ",
            max_sel_prop, ")", sep="")
        stop(err)
    }

    stopifnot(n_sel_clusts >= 1)

    # It may be impossible to get at least min_num_clusts or at most
    # max_num_clusts; if so, give a warning
    if(n_sel_clusts < min_num_clusts){
        warn <- paste("Returning fewer than min_num_clusts = ", min_num_clusts,
            " clusters because decreasing the cutoff any further would require returning more than max_num_clusts = ",
            max_num_clusts, " clusters", sep="")
        warning(warn)
    }
    if(!is.na(max_num_clusts)){
        if(n_sel_clusts > max_num_clusts){
            warn <- paste("Returning more than max_num_clusts = ",
                max_num_clusts,
                " clusters because increasing the cutoff any further would require returning 0 clusters",
                sep="")
            warning(warn)
        }
    }
}

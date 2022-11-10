# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to confirm that the argument prop_feats_remove to several 
#' functions is as expected
#'
#' @param prop_feats_remove Numeric; proportion of features that are dropped on
#' each subsample. Must be between 0 and 1.
#' @param p The number of features; must be greater than 2 if prop_feats_remove
#' is greater than 0.
#' @author Gregory Faletto, Jacob Bien
checkPropFeatsRemove <- function(prop_feats_remove, p){
    stopifnot(length(prop_feats_remove) == 1)
    stopifnot(is.numeric(prop_feats_remove) | is.integer(prop_feats_remove))
    stopifnot(!is.na(prop_feats_remove))
    stopifnot(prop_feats_remove >= 0 & prop_feats_remove < 1)
    if(prop_feats_remove > 0){
        # Make sure p is at least 2 or else this doesn't make sense
        stopifnot(p >= 2)
    }
}

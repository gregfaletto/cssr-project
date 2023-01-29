# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that the outputs of the provided feature selection
#' method are as required. 
#'
#' @param selected An integer vector; the indices of the features selected by
#' the lasso.
#' @param p The total number of observed features; all selected features must be
#' in 1:p.
#' @param feats_on_subsamp Integer; the indices of the features considered by
#' the feature selection method. All selected features must be among these
#' features.
#' @author Gregory Faletto, Jacob Bien
checkCssLoopOutput <- function(selected, p, feats_on_subsamp){
    if(!exists("selected")){
        stop("The provided feature selection method fitfun failed to return anything on (at least) one subsample")
    }
    if(!is.integer(selected) & !is.numeric(selected)){
        stop("The provided feature selection method fitfun failed to return an integer or numeric vector on (at least) one subsample")
    }
    if(any(is.na(selected))){
        stop("The provided feature selection method fitfun returned a vector containing NA values on (at least) one subsample")
    }
    if(!all(selected == round(selected))){
        stop("The provided feature selection method fitfun failed to return a vector of valid (integer) indices on (at least) one subsample")
    }
    if(length(selected) != length(unique(selected))){
        stop("The provided feature selection method fitfun returned a vector of selected features containing repeated indices on (at least) one subsample")
    }
    if(length(selected) > p){
        stop("The provided feature selection method fitfun returned a vector of selected features longer than p on (at least) one subsample")
    }
    if(length(selected) > 0){
        if(max(selected) > p){
            stop("The provided feature selection method fitfun returned a vector of selected features containing an index greater than ncol(X) on (at least) one subsample")
        }
        if(min(selected) <= 0){
            stop("The provided feature selection method fitfun returned a vector of selected features containing a non-positive index on (at least) one subsample")
        }
    }
    if("try-error" %in% class(selected) |
        "error" %in% class(selected) | "simpleError" %in% class(selected) |
        "condition" %in% class(selected)){
        stop("The provided feature selection method fitfun returned an error on (at least) one subsample")
    }
    if(!all(selected %in% feats_on_subsamp)){
        stop("The provided feature selection method somehow selected features that were not provided for it to consider.")
    }
}

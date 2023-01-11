# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to confirm that the argument weighting to several 
#' functions is as expected
#'
#' @param weighting Character; determines how to calculate the weights to
#' combine features from the selected clusters into weighted averages, called
#' cluster representatives. Must be one of "sparse", "weighted_avg", or
#' "simple_avg'.
#' @author Gregory Faletto, Jacob Bien
checkWeighting <- function(weighting){
    stopifnot(length(weighting)==1)
    stopifnot(!is.na(weighting))
    if(!is.character(weighting)){
        stop("Weighting must be a character")
    }
    if(!(weighting %in% c("sparse", "simple_avg", "weighted_avg"))){
        stop("Weighting must be a character and one of sparse, simple_avg, or weighted_avg")
    }
}

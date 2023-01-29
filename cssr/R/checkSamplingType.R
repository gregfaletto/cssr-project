# Generated from _main.Rmd: do not edit by hand

#' Helper function to confirm that the argument sampling_type to several 
#' functions is as expected
#'
#' @param sampling_type A character vector; either "SS" or "MB". "MB" is not
#' supported yet.
#' @author Gregory Faletto, Jacob Bien
checkSamplingType <- function(sampling_type){
    stopifnot(is.character(sampling_type))
    stopifnot(length(sampling_type) == 1)
    stopifnot(!is.na(sampling_type))
    stopifnot(sampling_type %in% c("SS", "MB"))
    if(sampling_type == "MB"){
        stop("sampling_type MB is not yet supported (and isn't recommended anyway)")
    }
}

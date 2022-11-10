# Generated from create-cssr.Rmd: do not edit by hand

#' Generate list of subsamples
#'
#` Generate list of `B` (or `2*B` for sampling_type="SS") subsamples of size
#` `n/2`
#' @param n Integer or numeric; sample size of the data set.
#' @param B Integer or numeric; the number of subsamples. Note: For
#' sampling.type=="MB" the number of lasso fits will be `B`; for
#' sampling_type="SS" the number of lasso fits will be `2*B`.
#' @param sampling_type A character vector (either "SS" or "MB"); the sampling
#' type used for stability selection.
#' @return A list of length `B` (or `2*B` for sampling_type="SS"), where each
#' element is an integer vector of length floor(`n/2`) containing the indices
#' of a subsample of 1:`n`. For sampling_type=="SS", the last `B` subsamples
#' will be complementary pairs of the first `B` subsamples (see Faletto and
#' Bien 2022 or Shah and Samworth 2013 for details).
#' @author Gregory Faletto, Jacob Bien
#' @references Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}. \cr Shah, R. D., & Samworth, R. J.
#' (2013). Variable selection with error control: Another look at stability
#' selection. \emph{Journal of the Royal Statistical Society. Series B:
#' Statistical Methodology}, 75(1), 55–80.
#' \url{https://doi.org/10.1109/RITA.2014.2302071}.
getSubsamps <- function(n, B, sampling_type){
    subsamples <- list()
    for(i in 1:B){
        subsamples[[i]] <- sort(sample.int(n=n, size=floor(n/2), replace=FALSE))
    }
    stopifnot(length(subsamples) == B)
    # TODO @gfaletto: add support for sampling_type="MS"
    if(sampling_type=="SS"){
        for(i in 1:B){
            # For the ith entry, take a subsample of size floor(n/2) from the
            # remaining n - floor(n/2) observations. (Only necessary to actually
            # take the subsample if n is odd; if not, the set we want is
            # setdiff(1:n, subsamples[[i]]), so skip the sample function.)
            if(n/2 == floor(n/2)){
                subsamples[[B + i]] <- sort(setdiff(1:n, subsamples[[i]]))
            } else{
                subsamples[[B + i]] <- sort(sample(x=setdiff(1:n,
                    subsamples[[i]]), size=floor(n/2), replace=FALSE))
            }

            # Check output

            stopifnot(is.integer(subsamples[[B + i]]))
            stopifnot(all(subsamples[[B + i]] ==
                round(subsamples[[B + i]])))
            stopifnot(floor(n/2) == length(subsamples[[B + i]]))
            stopifnot(length(subsamples[[B + i]]) ==
                length(unique(subsamples[[B + i]])))
        }
        stopifnot(length(subsamples) == 2*B)
    }
    return(subsamples)
}

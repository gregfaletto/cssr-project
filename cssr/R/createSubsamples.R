# Generated from create-cssr.Rmd: do not edit by hand

#' Creates lists of subsamples for stability selection.
#'
#' @param n Integer or numeric; sample size of the data set.
#' @param p Integer or numeric; number of features.
#' @param B Integer or numeric; the number of subsamples. Note: For
#' sampling.type=="MB" the number of lasso fits will be `B`; for
#' sampling_type="SS" the number of lasso fits will be `2*B`.
#' @param sampling_type A character vector (either "SS" or "MB"); the sampling
#' type used for stability selection.
#' @param num_feats_remove Integer; number of features select automatically on
#' every iteration. Determined earlier from input prop_feats_remove to css.
#' @return A list of length `B` (or `2*B` for sampling_type = "SS"). If
#' prop_feats_remove = 0, each list element is an integer vector of length
#' floor(`n/2`) containing the indices of a subsample of 1:`n`. (For
#' sampling_type=="SS", the last `B` subsamples will be complementary pairs of
#' the first `B` subsamples; see Faletto and Bien 2022 or Shah and Samworth 2013
#' for details.) If prop_feats_remove > 0, each element is a named list with
#' members "subsample" (same as above) and "feats_to_keep", a logical vector
#' of length p; feats_to_keep[j] = TRUE if feature j is chosen for this
#' subsample, and false otherwise.
#' @author Gregory Faletto, Jacob Bien
#' @references Faletto, G., & Bien, J. (2022). Cluster Stability Selection.
#' \emph{arXiv preprint arXiv:2201.00494}.
#' \url{https://arxiv.org/abs/2201.00494}. \cr Shah, R. D., & Samworth, R. J.
#' (2013). Variable selection with error control: Another look at stability
#' selection. \emph{Journal of the Royal Statistical Society. Series B:
#' Statistical Methodology}, 75(1), 55–80.
#' \url{https://doi.org/10.1109/RITA.2014.2302071}.
createSubsamples <- function(n, p, B, sampling_type, prop_feats_remove=0){

    # Check inputs

    stopifnot(length(n) == 1)
    stopifnot(is.numeric(n) | is.integer(n))
    stopifnot(n == round(n))
    stopifnot(n > 0)

    stopifnot(length(p) == 1)
    stopifnot(is.numeric(p) | is.integer(p))
    stopifnot(p == round(p))
    stopifnot(p > 0)

    checkB(B)
    checkSamplingType(sampling_type)
    checkPropFeatsRemove(prop_feats_remove, p)

    if(prop_feats_remove == 0){
        subsamples <- getSubsamps(n, B, sampling_type)
        return(subsamples)
    } else{
        # In this case, we generate subsamples as well as logical vectors
        # of features to keep
        subsamps_and_feats <- list()
        subsamples <- getSubsamps(n, B, sampling_type)
        for(i in 1:B){
            # Logical p-vector, where each entry is TRUE with probability
            # 1 - prop_feats_remove
            feats_to_keep_i <- as.logical(stats::rbinom(n=p, size=1,
                prob=1 - prop_feats_remove))
            # Make sure at least two entries are equal to TRUE (so that at
            # least two features are present for every subsample)--if not,
            # randomly choose features to add
            while(sum(feats_to_keep_i) < 2){
                false_inds <- which(!feats_to_keep_i)
                sel_feat <- sample(false_inds, size=1)
                feats_to_keep_i[sel_feat] <- TRUE
            }
            subsamps_and_feats[[i]] <- list(subsample=subsamples[[i]],
                feats_to_keep=feats_to_keep_i)
        }

        if(sampling_type=="SS"){
            stopifnot(length(subsamples) == 2*B)
            for(i in 1:B){
                # Keep the same features as in the other subsample (this
                # ensures that the theoretical guarantee of Shah and Samworth
                # 2013 remains valid on every individual pair of subsamples)
                subsamps_and_feats[[B + i]] <- list(subsample=subsamples[[B + i]],
                    feats_to_keep=subsamps_and_feats[[i]]$feats_to_keep)
            }
        }

        # Check output
        stopifnot(all(names(subsamps_and_feats) == c("subsample",
            "feats_to_keep")))

        return(subsamps_and_feats)
    }
    # Shouldn't be possible to reach this part of function
    stop("createSubsamples failed to return anything")
}

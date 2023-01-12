# Generated from create-cssr.Rmd: do not edit by hand

#' Helper function to confirm that the new X matrix provided to getCssDesign or
#' getCssPreds matches the characteristics of the X that was provided to css.
#'
#' @param trainX A numeric matrix (preferably) or a data.frame (which will
#' be coerced internally to a matrix by the function model.matrix). Must contain
#' the same features (in the same number of columns) as the X matrix provided to
#' css, and if the columns of trainX are labeled, the names must match the
#' variable names provided to css. trainX may be omitted if train_inds were
#' provided to css to set aside observations.
#' @param css_results An object of class "cssr" (the output of the function
#' css).
#' @return A named list with the following elements: \item{newX}{If trainX was
#' provided, this is the provided trainX matrix, coerced from a data.frame to a
#' matrix if the provided trainX was a data.frame. If trainX was not provided,
#' this is a matrix made up of the training indices provided to css in the
#' train_inds argument.} \item{newXProvided}{Logical; indicates whether a valid
#' trainX input was provided.}
#' @author Gregory Faletto, Jacob Bien
checkNewXProvided <- function(trainX, css_results){
    newXProvided <- FALSE

    if(all(!is.na(trainX)) & length(trainX) > 1){
        newXProvided <- TRUE
        trainX <- checkXInputResults(trainX, css_results$X)$newx
        
        n_train <- nrow(trainX)
        stopifnot(n_train > 1)
    } else{
        if(length(css_results$train_inds) == 0){
            stop("css was not provided with indices to set aside for model training (train_inds), so must provide new X in order to generate a design matrix")
        }
        trainX <- css_results$X[css_results$train_inds, ]
    } 
    stopifnot(is.matrix(trainX))
    stopifnot(is.numeric(trainX) | is.integer(trainX))
    stopifnot(all(!is.na(trainX)))
    stopifnot(ncol(trainX) >= 2)

    return(list(newX=trainX, newXProvided=newXProvided))
}

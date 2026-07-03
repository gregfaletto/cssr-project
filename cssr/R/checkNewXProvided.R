# Generated from _main.Rmd: do not edit by hand

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
#' @keywords internal
#' @noRd
checkNewXProvided <- function(trainX, css_results){
    newXProvided <- FALSE

    # "Was newX provided?" -- the documented default for trainX is the scalar NA
    # sentinel, so treat trainX as provided UNLESS it is exactly that default
    # (an atomic, length-1 NA). A previous length()-based heuristic
    # (length(trainX) > 1) misclassified a numeric vector or a one-column
    # data.frame (length 1) as "not provided" and silently fell back to
    # train_inds. The is.atomic/length/is.na test instead routes any matrix or
    # data.frame to the provided branch -- including an NA-CONTAINING matrix
    # (length > 1, so the is.na operand is short-circuited and never collapses a
    # multi-element matrix to a length-1 logical), which is then rejected by
    # checkXInputResults' checkNoNAs rather than silently replaced by the
    # train_inds data (#71).
    if(!(is.atomic(trainX) && length(trainX) == 1 && is.na(trainX))){
        # trainX was provided -- it must be a matrix or data.frame (a bare
        # vector is not a valid design and used to fail with a cryptic
        # is.matrix() stopifnot downstream).
        if(!(is.matrix(trainX) || is.data.frame(trainX))){
            stop("newX must be a matrix or data.frame with the same columns as the X provided to css().", call. = FALSE)
        }
        newXProvided <- TRUE
        trainX <- checkXInputResults(trainX, css_results$X)$newx
        
        n_train <- nrow(trainX)
        # A single-row newX is fine -- the design is just one row of cluster
        # representatives, and getCssPreds/cssPredict already accept a 1-row test
        # set -- so don't require more than one row here (#44).
        stopifnot(n_train >= 1)
    } else{
        if(length(css_results$train_inds) == 0){
            stop("css was not provided with indices to set aside for model training (train_inds), so must provide new X in order to generate a design matrix")
        }
        trainX <- css_results$X[css_results$train_inds, , drop = FALSE]
    } 
    stopifnot(is.matrix(trainX))
    stopifnot(is.numeric(trainX) | is.integer(trainX))
    stopifnot(all(!is.na(trainX)))
    stopifnot(ncol(trainX) >= 2)

    return(list(newX=trainX, newXProvided=newXProvided))
}

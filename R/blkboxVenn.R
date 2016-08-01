#' @export
.mpi <- function(results, Y){
  multi.plot.info = sapply(X = c(1:length(results$InnerFS)), results = results, y = Y, FUN = function(X, y, results){
    results$InnerFS[[X]]$Feature_Selection$FS.surviving.features[y]
  })
  names(multi.plot.info) = paste0(names(results$InnerFS))
  plot(Vennerable::Venn(multi.plot.info), doWeights = FALSE)
  grid::grid.text(paste0(names(results$InnerFS[[1]]$Feature_Selection$FS.surviving.features[Y])," NCV Feature Selection"), x=.5, y=.93)
  return(multi.plot.info)
}

#' Nested Crossfold Validation Venn Diagrams.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description Compares the features used in the holdouts of nested cross fold validation, this will tell you how much variation in features selected there is between holdouts.
#' @param results An object produced by the blkboxNCV function.
#' @keywords Venndiagram, Venn, Nested, blkbox, NCV
#' @examples
#' \donttest{
#' ncv.venn(blkboxNCV(...))
#'}
#' @export
#ncv venn plot function
ncv.venn <- function(results){

  if (!requireNamespace("Vennerable")) {
    stop("The Vennerable package is not installed. It can be installed by following the instructions at 'https://github.com/js229/Vennerable'")
  }

  plot.names = names(results$InnerFS$holdout_1$Feature_Selection$FS.surviving.features)
  out = sapply(X = seq_along(plot.names), results = results, FUN = function(X, results){
    .mpi(results, X)
  })
}

#' Crossfold Validation Venn Diagrams.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description This relies on numerous methods being used for feature selection in blkboxCV. It will construct a venn diagram to compare the features selected by various algorithms based on feature importance.
#' @param results An object produced by the blkboxCV function.
#' @keywords Venndiagram, Venn, crossfold, blkbox, CV
#' @examples
#'\donttest{
#' cv.venn(blkboxCV(..., Method = c("randomforest", "SVM"), AUC = 0.50))
#'}
#' @export
#cv venn plot function
cv.venn <- function(results){
  if (!requireNamespace("Vennerable", quietly = TRUE)) {
    stop("The Vennerable package is not installed. It can be installed by following the instructions at 'github.com/js229/Vennerable'")
  }
  plot(Vennerable::Venn(results$Feature_Selection$FS.surviving.features), doWeights = FALSE)
  grid::grid.text("CV Feature Selection - Method Overlap", x=.5, y=.93)
}


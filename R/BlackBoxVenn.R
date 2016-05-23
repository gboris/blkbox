#' @export
.mpi <- function(results, Y){
  multi.plot.info = sapply(X = c(1:length(results$InnerFS)), y = Y, FUN = function(X, y){
    results$InnerFS[[X]]$Feature_Selection$FS.surviving.features[Y]
  })
  names(multi.plot.info) = paste0(names(results$InnerFS))
  plot(Venn(multi.plot.info), doWeights = FALSE)
  grid.text(paste0(names(results$InnerFS[[1]]$Feature_Selection$FS.surviving.features[Y])," NCV Feature Selection"), x=.5, y=.93)
  return(multi.plot.info)
}

#' Nested Crossfold Validation Venn Diagrams
#'
#' @author Zachary Davies, Boris Guennewig
#' @description Compares the features used in the holdouts of nested cross fold validation, this will tell you how much variataion in features selected there is between holdouts.
#' @param results An object produced by the blkboxNCV function.
#' @keywords Venndiagram, Venn, Nested, blkbox, NCV
#' @export
#ncv venn plot function
ncv.venn <- function(results){
  plot.names = names(results$InnerFS$holdout_1$Feature_Selection$FS.surviving.features)
  out = sapply(X = seq_along(plot.names), results = results, FUN = function(X, results){
    .mpi(results, X)
  })
}

#' Crossfold Validation Venn Diagrams
#'
#' @author Zachary Davies, Boris Guennewig
#' @description This relies on numerous methods being used for feature selection in BlackBoxCV. It will construct a venn diagram to compare the features selected by various algorithms based on feature importance.
#' @param results An object produced by the blkboxcCV function.
#' @keywords Venndiagram, Venn, crossfold, blkbox, CV
#' @export
#cv venn plot function
cv.venn <- function(results){
  plot(Venn(results$Feature_Selection$FS.surviving.features), doWeights = FALSE)
  grid.text("CV Feature Selection - Method Overlap", x=.5, y=.93)
}


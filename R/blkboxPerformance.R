#' blkbox Performance.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description Determines the performance of each model within the blkbox or blkboxCV output. Can choose from a range of performance metrics.
#' @param object the blkboxCV or blkbox output
#' @param metric Which metric will be used for performance. Area under the Receiver operating curve = "AUROC", Accuracy = "ACC", Error rate = "ERR", Matthews correlation coefficient = "MCC", F-1 score = "F-1", Sensitivity or True Positive Rate = "TPR", Specificity or True Negative Rate = "TNR". default = "AUROC".
#' @param consensus if the process was repeated it will calculate the consensus vote for each sample across the repititons before then calculating the performance across all samples. Default is False.
#' @examples
#'\donttest{
#' Performance(blkbox(...), metric = "AUROC")
#' Performance(blkboxCV(...), metric = "ERR")
#'}
#' @keywords performance, blkbox, AUROC, F-1, ERR, MCC, ACC, TPR, TNR.
#' @export
Performance <- function(object, metric = "AUROC", consensus = FALSE){

  output <- list()
  avg.predicted <- list()
  metrics <- list()
  nums <- list()

  if (hasArg(object) & (names(object)[1] == "algorithm.votes")){
    results <- list()
    votes <- object$algorithm.votes
    labels <- as.numeric(factor(x = object$input.data$labels[, 1], labels = c(1, 2)))
  } else {
    stop("Need a blkbox Object to calculate importance")
  }

  for (m in 1:length(metric)){

    if (consensus == TRUE){
      z = 1
    } else {
      z = dim(votes[[1]])[1]
    }

    for (i in 1:length(votes)){
      performance = NULL
      for (q in 1:z){
        if (z != 1){
          avg.predicted[[q]] = votes[[i]][q, ]
        } else {
          avg.predicted[[q]] = colMeans(votes[[i]])
        }
        if (metric[m] == "AUROC"){

          ROC <- pROC::roc(predictor = as.numeric(as.matrix(avg.predicted[[q]])), response = labels, auc = TRUE)
          performance[q] <- ROC$auc
          nums[[length(nums)+1]] <- cbind(Rep = rep(paste0("Repeat ",q), length(ROC$specificities)),
                                          Algorithm = rep(names(votes)[i], length(ROC$specificities)),
                                          data.frame(cbind(rev(ROC$specificities), rev(ROC$sensitivities))))

        } else {
          TP <- length(which((round(avg.predicted[[q]])[which(as.numeric(round(avg.predicted[[q]])) == labels)]) == levels(as.factor(labels))[1]))
          FP <- length(which((round(avg.predicted[[q]])[which(as.numeric(round(avg.predicted[[q]])) != labels)]) == levels(as.factor(labels))[2]))
          FN <- length(which((round(avg.predicted[[q]])[which(as.numeric(round(avg.predicted[[q]])) != labels)]) == levels(as.factor(labels))[1]))
          TN <- length(which((round(avg.predicted[[q]])[which(as.numeric(round(avg.predicted[[q]])) == labels)]) == levels(as.factor(labels))[2]))
          N <- FP + TN
          P <- TP + FN
          if (metric[m] == "ACC"){
            performance[q] <- (TP + TN) / (P + N)
          } else if (metric[m] == "MCC"){
            performance[q] <- (TP * TN - FP * FN) / sqrt( (TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
          } else if (metric[m] == "TPR"){
            performance[q] <- TP / (TP + FN)
          } else if (metric[m] == "TNR"){
            performance[q] <- TN / (FN + TN)
          } else if (metric[m] == "F-1"){
            performance[q] <- (2 * TP) / ( (2 * TP) + FP + FN)
          } else if (metric[m] == "ERR"){
            performance[q] <- mean(round(avg.predicted[[q]]) != labels)
          } else {
            stop("Metric unsupported")
          }
        }
        performance[is.na(performance)] <- 0
        output[[names(votes)[i]]] <- performance

      }
    }
    metrics[[(metric[m])]] <- output
  }
  if ("AUROC" %in% metric){
    nums <- Reduce(rbind, nums)
    return(list("Performance" = metrics, "metric" = metric, "roc.values" = nums))
  } else {
    return(list("Performance" = metrics, "metric" = metric))
  }
}

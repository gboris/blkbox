#' BlackBox Performance
#'
#' @author Zachary Davies, Boris Guennewig
#' @description Determines the performance of each model within the BlackBox or BlackBoxCV output. Can choose from a range of performance metrics.
#' @param object
#' @param metric Which metric will be used for performance. Area under the Receiver operating curve = "AUROC", Accuracy = "ACC", Error rate = "ERR", Matthews correlation coeffecient = "MCC", F-1 score = "F-1".
#' @param consensus if the process was repeated it will calculate the consensus vote for each sample across the repititons before then calculating the performance across all samples. Default is False.
#' @keywords performance, blackbox, AUROC, F-1, ERR, MCC, ACC.
#' @export
Performance <- function(object, metric, consensus){

  output = list()
  avg.predicted = list()
  metrics = list()

  if(hasArg(object) & (names(object)[1] == "algorithm.votes")){
    results = list()
    votes = object$algorithm.votes
    labels = as.numeric(factor(x = object$input.data$labels[,1], labels = c(1,2)))
  } else {
    stop("Need a BlackBox Object to caculate importance")
  }

  if(!hasArg(metric)){
    metric = "AUROC"
    message("No metric chosen, defaulting to AUROC", "\n")
  }
  if(!hasArg(consensus)){
    consensus = FALSE
  }


  for(m in 1:length(metric)){

    if(consensus == TRUE){
      z = 1
    } else {
      z = dim(votes[[1]])[1]
    }

    for(i in 1:length(votes)){
      performance = NULL
      for(q in 1:z){
        if(z != 1){
          avg.predicted[[q]] = votes[[i]][q,]
        } else {
          avg.predicted[[q]] = colMeans(votes[[i]])
        }
        if(metric[m] == "AUROC"){
          ROC = pROC::roc(predictor = as.numeric(as.matrix(avg.predicted[[q]])), response = labels, auc = TRUE, ci = TRUE)
          performance[q] = ROC$auc
        } else {
          TP = length(which((round(avg.predicted[[q]])[which(as.numeric(round(avg.predicted[[q]])) == labels)]) == levels(as.factor(labels))[1]))
          FP = length(which((round(avg.predicted[[q]])[which(as.numeric(round(avg.predicted[[q]])) != labels)]) == levels(as.factor(labels))[2]))
          FN = length(which((round(avg.predicted[[q]])[which(as.numeric(round(avg.predicted[[q]])) != labels)]) == levels(as.factor(labels))[1]))
          TN = length(which((round(avg.predicted[[q]])[which(as.numeric(round(avg.predicted[[q]])) == labels)]) == levels(as.factor(labels))[2]))
          N = FP + TN
          P = TP + FN
          if(metric[m] == "ACC"){
            performance[q] = (TP + TN)/(P+N)
          } else if(metric[m] == "MCC"){
            performance[q] = (TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
          } else if(metric[m] == "F-1"){
            performance[q] = (2*TP)/((2*TP)+FP+FN)
          } else if(metric[m] == "ERR"){
            performance[q] = mean(round(avg.predicted[[q]]) != labels)
          } else {
            stop("Metric unsupported")
          }
        }
        output[[names(votes)[i]]] = performance
      }
    }
    metrics[[(metric[m])]] = output
  }

  return(list("Performance" = metrics, "metric" = metric))
}

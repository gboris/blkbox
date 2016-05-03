#' Nested cross fold validation with BlackBox
#'
#' @author Zachary Davies, Boris Guennewig
#' @description A function that builds upon the BlackBox and BlackBoxNCV function and performs nested k-fold cross validation and then provides votes for each fold as well as the importance of each feature in the models.
#' @param data A data.frame where the columns correspond to features and the rows are samples. The dataframe will be shuffled and split into k folds for downstream analysis.
#' @param labels A character or numeric vector of the class indetifiers that each sample belongs.
#' @param outerfolds The number of folds that will be in the first k-fold loop, this determines the number of holdouts.
#' @param innerfolds The number of folds that occur in the internal feature selection cross fold validation before testing on the corresponding holdout.
#' @param seeds A numeric vector
#' @param ntrees The number of trees used in the ensemble based learners (randomforest, bigrf, party, bartmachine). default = 500.
#' @param mTry The number of features sampled at each node in the trees of ensemble based learners (randomforest, bigrf, party, bartmachine). default = sqrt(number of features).
#' @param Kernel The type of kernel used in the support vector machine algorithm (linear, radial, sigmoid, polynomial). default = "linear".
#' @param Gamma Advanced parameter, defines the distance of which a single training example reaches. Low gamma will produce a SVM with softer boundries, as Gamma increases the boundries will eventually become restricted to their singular support vector.
#' @param exclude removes certain algorithms from analysis - to exclude random forest which you would set exclude = c(1). To only run GLM you would set exclude = c(1:4,6:8). The algorithms each have their own numeric identifier. randomforest = 1, knn = 2, bartmachine = 3, party = 4, glm = 5, pam = 6, nnet = 7, svm = 8.
#' @param inn.exclude removes certain algorithms from after feature selection analysis. similar to 'exclude'.
#' @param Method The algorithm used to feature select the data. Uses the feature importance from the algorithms to rank and remove anything below the AUC threshold.
#' @param AUC Area under the curve selection measure. The relative importance of features is calculated and then ranked. The features responsible for the most importance are therefore desired, the AUC value is the percentile in which to keep features above. 0.5 keeps the highest ranked features responsible for 50 percent of the cumulative importance.
#' @param metric A character string to determine which performance metric will be passed on to the Performance() function. Refer to Perfromance() documentation.
#' @keywords Cross Validation, k-fold, BlackBox, AUC, feature selection
#' @export
BlackBoxNCV <- function(data, labels, outerfolds, innerfolds, ntrees, mTry, Kernel, Gamma, exclude, inn.exclude, Method, AUC, metric){

  labels = as.numeric(factor(x = labels, labels = c(1,2)))
  class = data.frame(y = (c(labels)))
  class.data = cbind(data, class)
  actual.label = data.frame(labels = class.data$y, row.names = rownames(class.data))

  #########################
  nk = outerfolds
  k = innerfolds
  z = sample(runif(1:1000), 1)*100


  inner.feature.selection = list()
  inner.blackbox = list()
  inner.performance = list()
  holdout.result = list()
  holdout.result = list()
  holdout.performance = list()
  re.shuffle.counter = 0


  while(!exists("nfold_intervals")){
    set.seed(z)
    class.data = class.data[sample(nrow(class.data)),]
    #inital nested folds
    nfolds1 = nrow(class.data) %% nk
    nsize1 = floor(nrow(class.data)/nk+1)
    nfolds2 = nk-(nrow(class.data) %% nk)
    nsize2 = floor(nrow(class.data)/nk)

    nfold_intervals = c(seq(from = 0, to = nfolds1*nsize1, by = nsize1), seq(from = ((nfolds1*nsize1)+nsize2), to = (nfolds1*nsize1 + nfolds2*nsize2), by = nsize2))

    for(i in 1:nk){
      s1 = nfold_intervals[i]+1
      s2 = nfold_intervals[i+1]
      subset = s1:s2
      levels.test = class.data$y[subset]
      if(length(levels(as.factor(levels.test))) == 1){
        rm(nfold_intervals)
        re.shuffle.counter = re.shuffle.counter + 1
        break
      }
    }
  }
  cat("Folds was reshufled", re.shuffle.counter, "times.\n")
  if((nfolds1*nsize1 + nfolds2*nsize2) == nrow(class.data)){
    if(nfolds1 == 0){
      cat(nk, "nested fold cross validation, fold size:", nsize2, "\n")
    } else {
      cat(nk, "nested fold cross validation with", nfolds1, "folds of", nsize1, "and", nfolds2, "folds of", nsize2, "\n")
    }
  } else {
    stop("error in cross validation, set folds parameter","\n")
  }
  #########################
  if(!hasArg(data)){
    stop("Ensemble cannot run without data, provide data.frame of samples by features")
  }
  if(!hasArg(labels)){
    stop("Ensemble cannot run without class, provide to 'labels' parameter")
  } else {
    if(length(levels(as.factor(labels))) > 2){
      #stop("BlackBox does not support non-binary classification tasks")
    }
  }
  if(hasArg(Gamma)){
    svm.gamma = Gamma
  } else {
    svm.gamma = 1/(ncol(data)-1)
  }
  if(hasArg(m.try)){
    m.try = mTry
  } else {
    m.try = round(sqrt(ncol(data)))
  }
  if(hasArg(ntrees)){
    nTrees = ntrees
  } else {
    nTrees = 501
  }
  if(hasArg(folds) == FALSE){
    folds = 10
  }
  if(hasArg(nfolds) == FALSE){
    folds = 10
  }
  if(!hasArg(exclude)){
    exclude = c(0)
  }
  if(!hasArg(inn.exclude)){
    inn.exclude = c(0)
  }
  if(hasArg(Kernel)){
    svm.kernel = Kernel
  } else {
    svm.kernel = "linear"
    cat("No kernel provided, using linear kernel from e1071 package", "\n")
  }
  if(!hasArg(metric)){
    metric = "AUROC"
    cat("No metric chosen, defaulting to AUROC", "\n")
  }
  ########

  #set the seed and then shuffle the data accordingly

  #start loop - for the number of nested folds
  for(i in 1:nk){
    #splitting data
    s1 = nfold_intervals[i]+1
    s2 = nfold_intervals[i+1]
    subset = s1:s2
    ncv.train = class.data[-subset,]
    ncv.test = class.data[subset,]
    nclasstr = data.frame(condition = (factor(class.data$y[-subset])))
    nclassts = data.frame(condition = (factor(class.data$y[subset])))

    #we take nk-1 data portion and give to blackbox functions
    #BlackBoxFS to reduce the data nk times)
    inner.feature.selection[[paste0("holdout_",i)]] = BlackBoxCV(data = ncv.train[,-ncol(ncv.train)], labels = nclasstr$condition, folds = k, Method = Method, AUC = AUC, Gamma = svm.gamma, exclude = exclude)
    #BlackBox standard and performance to determine the inner loop performances
    inner.blackbox[[paste0("holdout_",i)]] = BlackBoxCV(data = inner.feature.selection[[paste0("holdout_",i)]]$Feature_Selection$FS.data, labels = nclasstr$condition, folds = k, exclude = exclude)
    inner.performance[[paste0("holdout_",i)]] = Performance(inner.blackbox[[paste0("holdout_",i)]], metric = metric, consensus = TRUE)
    #Store results feature selected subset, their importance, the inner fold performance

    #BlackBoxTrain on all BB_AFS$FS.data selected data and then BlackBoxPredict on ncv.test
    holdout.result[[paste0("holdout_",i)]] = BlackBox(data = inner.feature.selection[[paste0("holdout_",i)]]$Feature_Selection$FS.data, holdout = ncv.test, labels = nclasstr$condition, holdout.labels = nclassts$condition, Kernel = svm.kernel, Gamma = svm.gamma, mTry = m.try, ntrees = nTrees, exclude = inn.exclude)
    #performance of BlackBoxPredict result
    holdout.performance[[paste0("holdout_",i)]] = Performance(holdout.result[[paste0("holdout_",i)]], metric = metric, consensus = TRUE)
  }

  #Neaten up the performance of each holdout

  #Build table of importance of each feature average from each nested subset and then weight them by the performance of the holdout

  #for each algorithm, make a table
  #table will be calculated by going through each
  #return(list(holdout.result, holdout.performance))
  temp_table1 = list()
  temp_list1 = list()
  for(alg in 1:length(names(holdout.result[[1]][[2]]))){
    for(i in 1:length(holdout.result)){
      if(i == 1){
        temp_table1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]] = data.frame(t(holdout.result[[paste0("holdout_",i)]][[2]][[alg]]*holdout.performance[[i]][[1]][[metric[1]]][[alg]]), row.names = paste("Holdout_",i))
      } else {
        temp_table1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]] = plyr::rbind.fill(temp_table1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]], data.frame(t(holdout.result[[paste0("holdout_",i)]][[2]][[alg]]*holdout.performance[[paste0("holdout_",i)]][[1]][[metric[1]]][[alg]]), row.names = paste("Holdout_",i)))
      }
    }
    temp_table1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]][is.na(temp_table1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]])] <- 0
    temp_list1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]] = data.frame(ImpWeightedAvg = sort(colMeans(temp_table1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]]), decreasing = T))
  }

  merged_votes = list()
  for(q in 1:(8 - length(inn.exclude))){

    for(i in 1:nk){

      if(i == 1){

        merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]] = holdout.result[[paste0("holdout_",i)]]$algorithm.votes[[q]]

      } else {

        #bind
        merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]] = cbind(merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]], holdout.result[[paste0("holdout_",i)]]$algorithm.votes[[q]])

        if(i == nk){

          #order
          merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]] = t(as.matrix(merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]][, match(rownames(data), colnames(merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]]))]))

        }

      }

    }

  }

  #performance
  ncv_votes = list("algorithm.votes" = merged_votes, "input.data" = list("labels" = data.frame(labels)))
  ncv.perf = BlackBox::Performance(ncv_votes, metric = metric, consensus = F)



  #Output
  return(list("InnerFS" = inner.feature.selection, "InnerBB" = inner.blackbox, "InnerPerf" = inner.performance, "HoldoutRes" = holdout.result, "HoldoutPerf" = holdout.performance, "HoldoutPerfMerged" = ncv.perf, "FeatureTables" = temp_table1, "WeightedAverageImportance" = temp_list1, repeat.num = repea))

}

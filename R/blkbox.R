#' Train and Test datasets.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description This standard function will allow multiple machine learning algorithms to be utilized on the same data to determine, which algorithm may be the most appropriate.
#' @param data a data frame of training data where the features correspond to columns and the samples are rows. As data size increases the memory required and run time of some algorithms may compound exponentially.
#' @param a character or numeric vector that contains the training class identifiers for the samples in the data frame. Must appear in the same order.
#' @param holdout a data frame of holdout of testing data where the features correspond to columns and the samples are the rows.
#' @param a character or numeric vector that contains the holdout or testing class identifiers for the samples in the holdout data frame.
#' @param ntrees The number of trees used in the ensemble based learners (randomforest, bigrf, party, bartmachine). default = 500.
#' @param mTry The number of features sampled at each node in the trees of ensemble based learners (randomforest, bigrf, party, bartmachine). default = sqrt(number of features).
#' @param Kernel The type of kernel used in the support vector machine algorithm (linear, radial, sigmoid, polynomial). default = "linear".
#' @param Advanced parameter, defines the distance of which a single training example reaches. Low gamma will produce a SVM with softer boundaries, as Gamma increases the boundaries will eventually become restricted to their singular support vector. default is 1/(ncol - 1).
#' @param exclude  removes certain algorithms from analysis - to exclude random forest which you would set exclude = c(1). To only run GLM you would set exclude = c(1:4,6:8). The algorithms each have their own numeric identifier. randomforest = 1, knn = 2, bartmachine = 3, party = 4, glm = 5, pam = 6, nnet = 7, svm = 8.
#' @keywords Machine Learning, blkbox, Training, Testing
#' @export
blkbox <- function(data, labels, holdout, holdout.labels, ntrees, mTry, Kernel, Gamma, exclude, keep.input){

  startMem = mem_used()
  startTime = Sys.time()

  if(!hasArg(data)){
    stop("Ensemble cannot run without data, provide data.frame of samples by features")
  }

  if(!hasArg(labels)){
    stop("Ensemble cannot run without class, provide to 'labels' parameter")
  } else {
    if(length(levels(as.factor(labels))) > 2){
      stop("blkbox does not support non-binary classification tasks")
    }
  }

  #class will appropraite the labels into a data frame
  #The data from feature selected data will not contain a response column and therefore will need to be bound
  labels = as.numeric(factor(x = labels, labels = c(1,2)))
  class = data.frame(y = (c(labels)))
  class.data = cbind(data, class)
  actual.label = data.frame(labels = class.data$y, row.names = rownames(class.data))

  cv.train = class.data
  classtr = data.frame(condition = factor(cv.train$y))

  #reduce the holdout set to the same features as decicided upon in feature selection


  class_ho = data.frame(y = (c(holdout.labels)))
  cv.test = holdout[, which(colnames(holdout) %in% colnames(data))]
  cv.test = cbind(cv.test, class_ho)
  classts = data.frame(condition = factor(holdout.labels))

  #Creating lists for data storage
  algorithm.importance = list()
  algorithm.votes = list()
  algorithm_list = list()

  if(!hasArg(exclude)){
    exclude = c(0)
  }

  if(ncol(class.data) > 4001){
    tree.method = 1
    if(1 %in% exclude == FALSE){
      #message("Large number of features detected, using the bigrf package", "\n")
    }
  } else {
    tree.method = 0
    if(1 %in% exclude == FALSE){
      #message("Using randomForest package", "\n")
    }
  }



  if(hasArg(Kernel)){
    svm.kernel = Kernel
  } else {
    svm.kernel = "linear"
  }
  if(hasArg(Gamma)){
    svm.gamma = Gamma
  } else {
    svm.gamma = 1/(ncol(class.data)-1)
  }
  if(hasArg(m.try)){
    m.try = mTry
  } else {
    m.try = round(sqrt(ncol(class.data)))
  }
  if(hasArg(ntrees)){
    nTrees = ntrees
  } else {
    nTrees = 501
  }



  if(1 %in% exclude == FALSE){
    if(tree.method > 0){
      algorithm_list[["randomforest"]] = .BB_BRF(cv.train = cv.train, cv.test = cv.test, classtr = classtr, classts = classts, m.try = m.try, nTrees = nTrees)
    } else {
      algorithm_list[["randomforest"]] = .BB_RF(cv.train = cv.train, cv.test = cv.test, classtr = classtr, m.try = m.try, nTrees = nTrees)
    }
  }
  if(2 %in% exclude == FALSE){
    algorithm_list[["kknn"]] = .BB_KKNN(cv.train = cv.train, cv.test = cv.test, distance = dist)
  }
  if(3 %in% exclude == FALSE){
    algorithm_list[["bartmachine"]] = .BB_BARTM(cv.train = cv.train, cv.test = cv.test, nTrees = nTrees)
  }
  if(4 %in% exclude == FALSE){
    algorithm_list[["party"]] = .BB_PARTY(cv.train = cv.train, cv.test = cv.test, m.try = m.try, nTrees = nTrees)
  }
  if(5 %in% exclude == FALSE){
    algorithm_list[["GLM"]] = .BB_GLM(cv.train = cv.train, cv.test = cv.test)
  }
  if(6 %in% exclude == FALSE){
    algorithm_list[["PamR"]] = .BB_PAM(cv.train = cv.train, cv.test = cv.test)
  }
  if(7 %in% exclude == FALSE){
    algorithm_list[["nnet"]] = .BB_NNET(cv.train = cv.train, cv.test = cv.test)
  }
  if(8 %in% exclude == FALSE){
    algorithm_list[["SVM"]] = .BB_SVM(cv.train = cv.train, cv.test = cv.test, classtr = classtr, svm.kernel = svm.kernel, svm.gamma = svm.gamma)
  }


  for(q in 1:length(algorithm_list)){
    if(names(algorithm_list)[q] != "kknn"){
      algorithm.importance[[names(algorithm_list)[q]]] = algorithm_list[[q]]$IMP
    }
    algorithm.votes[[names(algorithm_list)[q]]] = as.matrix(algorithm_list[[q]]$VOTE)
  }

  endTime = Sys.time()
  endMem = mem_used()
  diffMem = endMem - startMem
  elapsedTime = endTime - startTime

  return(list("algorithm.votes" = algorithm.votes, "algorithm.importance" = algorithm.importance, benchmarks = list("time" = elapsedTime, "memory.used" = diffMem)))

}

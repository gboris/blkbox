#' Train and Test datasets.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description This standard function will allow multiple machine learning algorithms to be utilized on the same data to determine, which algorithm may be the most appropriate.
#' @param data Data partitioned by into a list or a data frame of training data where the features correspond to columns and the samples are rows. As data size increases the memory required and run time of some algorithms may compound exponentially.
#' @param labels a character or numeric vector that contains the training class identifiers for the samples in the data frame. Must appear in the same order. Does not need to be specified if using a partitoned data list.
#' @param holdout a data frame of holdout of testing data where the features correspond to columns and the samples are the rows. Does not need to be specified if using a partitoned data list.
#' @param holdout.labels a character or numeric vector that contains the holdout or testing class identifiers for the samples in the holdout data frame. Does not need to be specified if using a partitoned data list.
#' @param ntrees The number of trees used in the ensemble based learners (randomforest, bigrf, party, bartmachine). default = 500.
#' @param mTry The number of features sampled at each node in the trees of ensemble based learners (randomforest, bigrf, party, bartmachine). default = sqrt(number of features).
#' @param Kernel The type of kernel used in the support vector machine algorithm (linear, radial, sigmoid, polynomial). default = "linear".
#' @param Gamma dvanced parameter, defines the distance of which a single training example reaches. Low gamma will produce a SVM with softer boundaries, as Gamma increases the boundaries will eventually become restricted to their singular support vector. default is 1/(ncol - 1).
#' @param exclude removes certain algorithms from analysis - to exclude random forest which you would set exclude = "randomforest". The algorithms each have their own numeric identifier. randomforest = "randomforest", knn = "kknn", bartmachine = "bartmachine", party = "party", glmnet = "GLM", pam = "PamR, nnet = "nnet", svm = "SVM.
#' @param seed Sets the seed for the bartMachine model.
#' @keywords Machine Learning, blkbox, Training, Testing
#' @importFrom methods hasArg
#' @export
blkbox <- function(data, labels, holdout, holdout.labels, ntrees, mTry, Kernel, Gamma, exclude, seed){

  startMem <- pryr::mem_used()
  startTime <- Sys.time()

  if(!hasArg(data)){
    stop("Provide training and holdout data appropriately, partitoned data lists or seperate parameters are accepted. See ?blkbox")
  }

  if(class(data) == "list"){

    labels = data$training.labels
    holdout.labels = data$holdout.labels
    holdout = data$holdout.data
    data = data$training.data

  } else {

    if (!hasArg(labels) || !hasArg(holdout) || !hasArg(holdout.labels)){
      stop("Provide all necessary data inputs")
    }

  }

  if (length(levels(as.factor(labels))) != 2){
    stop("blkbox does not support non-binary classification tasks")
  }

  if (!hasArg(seed)){
    seed = sample(1:1000, 1)
  }

  #class will appropraite the labels into a data frame
  #The data from feature selected data will not contain a response column and therefore will need to be bound
  labels <- ifelse(as.factor(labels) == levels(as.factor(labels))[1], 1, 2)
  class <- data.frame(y = (c(labels)))
  class.data <- cbind(data, class)
  actual.label <- data.frame(labels = class.data$y, row.names = rownames(class.data))


  cv.train <- class.data
  classtr <- data.frame(condition = factor(cv.train$y))

  #reduce the holdout set to the same features as decicided upon in feature selection
  holdout.labels <- ifelse(as.factor(holdout.labels) == levels(as.factor(holdout.labels))[1], 1, 2)
  class_ho <- data.frame(y = (c(holdout.labels)))
  cv.test <- holdout[, which(colnames(holdout) %in% colnames(data))]
  cv.test <- cbind(cv.test, class_ho)
  classts <- data.frame(condition = factor(cv.test$y))
  #Creating lists for data storage
  algorithm.importance <- list()
  algorithm.votes <- list()
  algorithm_list <- list()
  if (!hasArg(exclude)){
    exclude = c(0)
  }

  if (ncol(data) > 4001){
    tree.method = 1
    if ("randomforest" %in% exclude == FALSE){
      if (!requireNamespace("bigrf", quietly = TRUE)) {
        message("The bigrf package is not installed.\nWithout this analysis with randomforest for large numbers of features is very slow.\nInstall it via 'devtools::install_github('aloysius-lim/bigrf')'")
        tree.method = 0
      }
    }
  } else {
    tree.method = 0
  }

  if (hasArg(Kernel)){
    svm.kernel = Kernel
  } else {
    svm.kernel = "linear"
  }
  if (hasArg(Gamma)){
    svm.gamma = Gamma
  } else {
    svm.gamma = 1/(ncol(data)-1)
  }
  if (hasArg(m.try)){
    m.try = mTry
  } else {
    m.try = round(sqrt(ncol(data)))
  }
  if (hasArg(ntrees)){
    nTrees = ntrees
  } else {
    nTrees = 501
  }



  if ("randomforest" %in% exclude == FALSE){
    if ( tree.method > 0){
      algorithm_list[["randomforest"]] = .BB_BRF(cv.train = cv.train, cv.test = cv.test, classtr = classtr, classts = classts, m.try = m.try, nTrees = nTrees)
    } else {
      algorithm_list[["randomforest"]] = .BB_RF(cv.train = cv.train, cv.test = cv.test, classtr = classtr, m.try = m.try, nTrees = nTrees)
    }
  }
  if ("kknn" %in% exclude == FALSE){
    algorithm_list[["kknn"]] = .BB_KKNN(cv.train = cv.train, cv.test = cv.test)
  }
  if ("bartmachine" %in% exclude == FALSE){
    algorithm_list[["bartmachine"]] = .BB_BARTM(cv.train = cv.train, cv.test = cv.test, nTrees = nTrees, seed = seed)
  }
  if ("party" %in% exclude == FALSE){
    algorithm_list[["party"]] = .BB_PARTY(cv.train = cv.train, cv.test = cv.test, m.try = m.try, nTrees = nTrees)
  }
  if ("GLM" %in% exclude == FALSE){
    algorithm_list[["GLM"]] = .BB_GLM(cv.train = cv.train, cv.test = cv.test)
  }
  if ("PamR" %in% exclude == FALSE){
    algorithm_list[["PamR"]] = .BB_PAM(cv.train = cv.train, cv.test = cv.test)
  }
  if ("nnet" %in% exclude == FALSE){
    algorithm_list[["nnet"]] = .BB_NNET(cv.train = cv.train, cv.test = cv.test)
  }
  if ("SVM" %in% exclude == FALSE){
    algorithm_list[["SVM"]] = .BB_SVM(cv.train = cv.train, cv.test = cv.test, classtr = classtr, svm.kernel = svm.kernel, svm.gamma = svm.gamma)
  }


  for (q in 1:length(algorithm_list)){
    if (names(algorithm_list)[q] != "kknn" || (names(algorithm_list)[q] != "SVM" & svm.kernel != "linear")){
      algorithm.importance[[names(algorithm_list)[q]]] = algorithm_list[[q]]$IMP
    }
    algorithm.votes[[names(algorithm_list)[q]]] = as.matrix(algorithm_list[[q]]$VOTE)
  }

  endTime <- Sys.time()
  endMem <- pryr::mem_used()
  diffMem <- endMem - startMem
  elapsedTime <- endTime - startTime

  return(list("algorithm.votes" = algorithm.votes, "algorithm.importance" = algorithm.importance, benchmarks = list("time" = elapsedTime, "memory.used" = diffMem), "input.data" = list("Data" = class.data ,"labels" = classts)))

}

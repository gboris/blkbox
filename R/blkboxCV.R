#' k-fold cross validation with blkbox.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description A function that builds upon the blkbox function and performs k-fold cross validation and then provides votes for each fold as well as the importance of each feature in the models.
#' @param data A data.frame where the columns correspond to features and the rows are samples. The dataframe will be shuffled and split into k folds for downstream analysis.
#' @param labels A character or numeric vector of the class identifiers that each sample belongs.
#' @param folds The number of times the data set will be subsectioned (number of samples / k, if modulo exists the groups will be as close to the same size as possible). Each data subsection will be used as a holdout portion. default = 10.
#' @param seed A numeric value. defaults to a randomly generated set of seeds that are output when run starts.
#' @param ntrees The number of trees used in the ensemble based learners (randomforest, bigrf, party, bartmachine). default = 500.
#' @param mTry The number of features sampled at each node in the trees of ensemble based learners (randomforest, bigrf, party, bartmachine). default = sqrt(number of features).
#' @param repeats repeat the cross validation process. default = 1.
#' @param Kernel The type of kernel used in the support vector machine algorithm (linear, radial, sigmoid, polynomial). default = "linear".
#' @param Gamma Advanced parameter, defines the distance of which a single training example reaches. Low gamma will produce a SVM with softer boundaries, as Gamma increases the boundaries will eventually become restricted to their singular support vector. default is 1/(ncol - 1).
#' @param exclude removes certain algorithms from analysis - to exclude random forest which you would set exclude = "randomforest". The algorithms each have their own numeric identifier. randomforest = "randomforest", knn = "kknn", bartmachine = "bartmachine", party = "party", glmnet = "GLM", pam = "PamR, nnet = "nnet", svm = "SVM", xgboost = "xgboost".
#' @param Method The algorithm used to feature select the data. Uses the feature importance from the algorithms to rank and remove anything below the AUC threshold. Default is "GLM".
#' @param AUC Area under the curve selection measure. The relative importance of features is calculated and then ranked. The features responsible for the most importance are therefore desired, the AUC value is the percentile in which to keep features above. 0.5 keeps the highest ranked features responsible for 50 percent of the cumulative importance. Default is NA which means feature are not selected at after CV. Will default to 1.0 if Method is "xgboost".
#' @examples
#'\donttest{
#' model_2 <- blkboxCV(data = my_data, labels = my_labels)
#'}
#' @keywords Cross Validation, k-fold, blkbox, AUC, feature selection
#' @importFrom stats runif
#' @export
blkboxCV <- function(data, labels, folds = 10, seed, ntrees, mTry, repeats = 1, Kernel, Gamma, max.depth, xgtype = "binary:logistic", exclude = c(0), Method = "GLM", AUC = "NA"){

   if (is.numeric(AUC) == FALSE & AUC != "NA"){
    stop("AUC must be numeric")
   }

  if (!hasArg(data)){
    stop("Ensemble cannot run without data, provide data.frame of samples by features")
  }
  if(ncol(data) < 2){
    stop("Data cannot a single feature")
  }
  if (!hasArg(labels)){
    stop("Ensemble cannot run without class, provide to 'labels' parameter")
  } else {
    if (length(levels(as.factor(labels))) != 2){
      stop("blkbox does not support non-binary classification tasks")
    }
  }

  if (hasArg(seed)){
    set.seed(seed)
    seed.list = sample(1000, repeats)
  } else {
    set.seed(sample(1000, 1))
    seed.list = sample(1000, repeats)
    message("No seed list provided, seeds are: ", toString(seed.list))
  }

  if (hasArg(ntrees)){
    nTrees = ntrees
  } else {
    nTrees = 501
  }

  if (hasArg(m.try)){
    m.try = mTry
  } else {
    m.try = round(sqrt(ncol(data)))
  }

  if (!hasArg(max.depth)){
    max.depth = round(sqrt(ncol(data)))
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

  startTime <- Sys.time()

  labels <- ifelse(as.factor(labels) == levels(as.factor(labels))[1], 1, 2)
  class <- data.frame(y = (c(labels)))
  class.data <- cbind(data, class)
  actual.label <- data.frame(labels = class.data$y, row.names = rownames(class.data))
  algorithm.importance <- list()
  algorithm.votes <- list()
  perf <- list()


    k <- folds
    folds1 <- nrow(class.data) %% k
    size1 <- floor(nrow(class.data) / k + 1)
    folds2 <- k - (nrow(class.data) %% k)
    size2 <- floor(nrow(class.data) / k)
    if ( (folds1 * size1 + folds2 * size2) == nrow(class.data)){
      if (folds1 == 0){
        message(k, " fold cross validation, fold size: ", size2)
      } else {
        message(k, " fold cross validation with ", folds1, " folds of ", size1, " and ", folds2, " folds of ", size2)
      }
    } else {
      stop("error in cross validation, set folds parameter")
    }
    fold_intervals = c(seq(from = 0, to = folds1*size1, by = size1), seq(from = ((folds1*size1)+size2), to = (folds1*size1 + folds2*size2), by = size2))

  counter = 0
  RunThrough = 0
  for(z in seed.list){

    set.seed(z)
    RunThrough = RunThrough + 1
    class.data = class.data[sample(nrow(class.data)),]

    for (i in 1:k){
      counter <- counter + 1
      s1 <- fold_intervals[i] + 1
      s2 <- fold_intervals[i + 1]
      subset <- s1:s2
      cv.train <- class.data[-subset, -ncol(class.data)]
      cv.test <- class.data[subset, -ncol(class.data)]
      classtr <- data.frame(condition = (factor(class.data$y[-subset])))
      classts <- data.frame(condition = (factor(class.data$y[subset])))

      BB_S <- blkbox(data = cv.train, labels = classtr$condition, holdout = cv.test, holdout.labels = classts$condition, ntrees = nTrees, mTry = m.try, Kernel = svm.kernel, Gamma = svm.gamma, exclude = exclude, seed = z, max.depth = max.depth, xgtype = xgtype)

      for (q in 1:length(names(BB_S$algorithm.votes))){

        if (names(BB_S$algorithm.votes)[q] != "kknn" || (names(BB_S$algorithm.votes)[q] != "SVM" & svm.kernel != "linear")){
          if (i == 1 & z == seed.list[1]){
            algorithm.importance[[names(BB_S$algorithm.votes)[q]]] = as.matrix(t(data.frame(BB_S$algorithm.importance[[names(BB_S$algorithm.votes)[q]]][,1], row.names = rownames(BB_S$algorithm.importance[[names(BB_S$algorithm.votes)[q]]]))))
          } else {
            algorithm.importance[[names(BB_S$algorithm.votes)[q]]] = plyr::rbind.fill.matrix(algorithm.importance[[names(BB_S$algorithm.votes)[q]]], as.matrix(t(data.frame(BB_S$algorithm.importance[[names(BB_S$algorithm.votes)[q]]][,1], row.names = rownames(BB_S$algorithm.importance[[names(BB_S$algorithm.votes)[q]]])))))
            # colnames(algorithm.importance[[names(BB_S$algorithm.votes)[q]]])[counter] <- paste0("R",RunThrough,"F",i)
            if (i == k & z == seed.list[repeats]){
              algorithm.importance[[names(BB_S$algorithm.votes)[q]]][is.na(algorithm.importance[[names(BB_S$algorithm.votes)[q]]])] = 0
              algorithm.importance[[names(BB_S$algorithm.votes)[q]]] = rbind(algorithm.importance[[names(BB_S$algorithm.votes)[q]]], colMeans(algorithm.importance[[names(BB_S$algorithm.votes)[q]]]))
              algorithm.importance[[names(BB_S$algorithm.votes)[q]]] = t(algorithm.importance[[names(BB_S$algorithm.votes)[q]]])
              colname_vec = sapply(c(1:(repeats * folds)), z = expand.grid(1:folds, 1:repeats), FUN = function(x, z){ paste0("R", z[x,2], "F", z[x,1]) })
              colnames(algorithm.importance[[names(BB_S$algorithm.votes)[q]]]) = c(colname_vec, "average")
            }
          }
        }
        if (i == 1){
          algorithm.votes[[names(BB_S$algorithm.votes)[q]]] = BB_S$algorithm.votes[[q]]
        } else {
          algorithm.votes[[names(BB_S$algorithm.votes)[q]]] = cbind(algorithm.votes[[names(BB_S$algorithm.votes)[q]]], BB_S$algorithm.votes[[q]])
          if (i == k){
            algorithm.votes[[names(BB_S$algorithm.votes)[q]]] = algorithm.votes[[names(BB_S$algorithm.votes)[q]]][, match(rownames(data), colnames(algorithm.votes[[names(BB_S$algorithm.votes)[q]]]))]
            if (z == 1){
              algorithm.votes[[paste0("vec_votes_", names(BB_S$algorithm.votes)[q])]] = algorithm.votes[[names(BB_S$algorithm.votes)[q]]]
              if (z == seed.list[repeats]){
                algorithm.votes[[names(BB_S$algorithm.votes)[q]]] = algorithm.votes[[paste0("vec_votes_", names(BB_S$algorithm.votes)[q])]]
                algorithm.votes[[paste0("vec_votes_", names(BB_S$algorithm.votes)[q])]] = NULL
              }
            } else {
              algorithm.votes[[paste0("vec_votes_", names(BB_S$algorithm.votes)[q])]] = rbind(algorithm.votes[[paste0("vec_votes_", names(BB_S$algorithm.votes)[q])]], algorithm.votes[[names(BB_S$algorithm.votes)[q]]])
              if (z == seed.list[repeats]){
                algorithm.votes[[names(BB_S$algorithm.votes)[q]]] = algorithm.votes[[paste0("vec_votes_", names(BB_S$algorithm.votes)[q])]]
                algorithm.votes[[paste0("vec_votes_", names(BB_S$algorithm.votes)[q])]] = NULL
              }
            }
          }
        }
      }
      message("Shuffle: ",RunThrough, " of ", repeats ," ","Fold: ",i)
    }
  }

  #Feature Selection Process
  if (AUC != "NA"){

    Output <- list()
    surviving.features <- list()
    surviving.features.importance <- list()

    for (s in Method){
      #Creating a data frame from average importance
      imp.frame <- data.frame(rowMeans(algorithm.importance[[s]][,-ncol(algorithm.importance[[s]])]))
      names(imp.frame) <- paste0(s, "_importance")
      #imp.frame = imp.frame[sort.list(imp.frame[,1], decreasing = T),,drop=F]
      if (s == "GLM"){
        GLM_adjust <- abs((imp.frame[,1]) - (as.numeric(names(sort(-table(imp.frame[,1])))[1])))
        imp.frame <- cbind(imp.frame, "GLM_importance" = GLM_adjust)[, -1, drop = F]
      }
      imp.frame <- data.frame(apply(imp.frame, 2, function(imp.frame) abs(imp.frame) * 100 / max(abs(imp.frame))))
      #Sorting the data frame, descending
      imp.frame <- imp.frame[sort.list(imp.frame[,1], decreasing = T),,drop=F]
      imp.frame <- cbind(feature_id = rownames(imp.frame), imp.frame)
      rownames(imp.frame) <- NULL
      #Append results to output list
      Output[[s]] <- imp.frame
    }

    for (d in 1:length(Output)){
      auc.max.value <- sum(Output[[d]][, 2]) * AUC
      cum.imp <- cumsum(Output[[d]][, 2])
      imp.range.max <- which(abs(cum.imp - auc.max.value) == min(abs(cum.imp - auc.max.value)))[1]
      imp.auc.cutoff <- (Output[[d]][, 2])[imp.range.max]
      #Drop features below imp.auc.cutoff
      imp.features <- Output[[d]][which(Output[[d]][,2] >= imp.auc.cutoff), , drop = F]
      surviving.features[[names(Output)[d]]] <- as.character(imp.features$feature_id)
      surviving.features.importance[[names(Output)[d]]] <- imp.features
    }

    if (length(Method) == 1){
      FS.data = data[, which(colnames(data) %in% surviving.features[[Method[1]]]), drop = F]
    } else {
      FS.data = data[, which(colnames(data) %in% unique(surviving.features[[Method[1]]], surviving.features[[Method[2]]])), drop = F]
    }
  }

  endTime <- Sys.time()
  elapsedTime <- endTime - startTime

  if (AUC != "NA"){
    return(list("algorithm.votes" = algorithm.votes,
                "algorithm.importance" = algorithm.importance,
                "Feature_Selection" = list("FS.data" = FS.data,
                                           "FS.surviving.features" = surviving.features,
                                           "FS.surviving.features.importance" = surviving.features.importance,
                                           "algorithm.importance" = Output,
                                           "importance.cutoff" = imp.auc.cutoff),
                "benchmarks" = list("time" = elapsedTime),
                "input.data" = list("Data" = class.data ,
                                    "labels" = actual.label)))
  } else {
    return(list("algorithm.votes" = algorithm.votes,
                "algorithm.importance" = algorithm.importance,
                "benchmarks" = list("time" = elapsedTime),
                "input.data" = list("Data" = class.data,
                                    "labels" = actual.label)))
  }

}


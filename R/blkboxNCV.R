#' Nested cross fold validation with blkbox.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description A function that builds upon the blkbox and blkboxNCV function and performs nested k-fold cross validation and then provides votes for each fold as well as the importance of each feature in the models. Provides feature importance tables and details for each inner and outerfold run.
#' @param data A data.frame where the columns correspond to features and the rows are samples. The dataframe will be shuffled and split into k folds for downstream analysis.
#' @param labels A character or numeric vector of the class identifiers that each sample belongs.
#' @param outerfolds The number of folds that will be in the first k-fold loop, this determines the number of holdouts. Default is 5.
#' @param innerfolds The number of folds that occur in the internal feature selection cross fold validation before testing on the corresponding holdout. Default is 5.
#' @param ntrees The number of trees used in the ensemble based learners (randomforest, bigrf, party, bartmachine). default = 500.
#' @param mTry The number of features sampled at each node in the trees of ensemble based learners (randomforest, bigrf, party, bartmachine). default = sqrt(number of features).
#' @param Kernel The type of kernel used in the support vector machine algorithm (linear, radial, sigmoid, polynomial). default = "linear".
#' @param Gamma Advanced parameter, defines the distance of which a single training example reaches. Low gamma will produce a SVM with softer boundaries, as Gamma increases the boundaries will eventually become restricted to their singular support vector. default is 1/(ncol - 1).
#' @param exclude removes certain algorithms from analysis - to exclude random forest which you would set exclude = "randomforest". The algorithms each have their own numeric identifier. randomforest = "randomforest", knn = "kknn", bartmachine = "bartmachine", party = "party", glmnet = "GLM", pam = "PamR, nnet = "nnet", svm = "SVM", xgboost = "xgboost".
#' @param inn.exclude removes certain algorithms from after feature selection analysis. similar to 'exclude'. Defaults to exclude all but Method.
#' @param Method The algorithm used to feature select the data. Uses the feature importance from the algorithms to rank and remove anything below the AUC threshold. Defaults to "GLM", therefore the inner folds will use "GLM" only unless specified otherwise.
#' @param AUC Area under the curve selection measure. The relative importance of features is calculated and then ranked. The features responsible for the most importance are therefore desired, the AUC value is the percentile in which to keep features above. 0.5 keeps the highest ranked features responsible for 50 percent of the cumulative importance. default = 0.5. Will Change to 1.0 default when Method = "xgboost".
#' @param metric A character string to determine which performance metric will be passed on to the Performance() function. Refer to Performance() documentation. default = c("ERR", "AUROC", "ACC", "MCC", "F-1")
#' @param seed A single numeric value that will determine all subsequent seeds set in NCV.
#' @keywords Cross Validation, k-fold, blkbox, AUC, feature selection
#' @importFrom stats runif
#' @importFrom magrittr "%>%"
#' @importFrom stats predict
#' @import ggplot2
#' @export
blkboxNCV <- function(data, labels, outerfolds = 5, innerfolds = 5, ntrees, mTry, Kernel, Gamma, max.depth, xgtype = "binary:logistic", exclude = c(0), inn.exclude, Method = "GLM", AUC = 0.5, metric = c("ERR", "AUROC", "ACC", "MCC", "F-1"), seed){

  . <- "cheeky"

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
  if(Method == "xgboost" && AUC < 0.5){
    message("xgboost does not assign many features importance and a higher AUC may be more appropraite")
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
  if (!hasArg(max.depth)){
    max.depth = round(sqrt(ncol(data)))
  }
  if (hasArg(ntrees)){
    nTrees = ntrees
  } else {
    nTrees = 501
  }
  if (!hasArg(inn.exclude)){
    alg_names = c("randomforest", "kknn", "bartmachine", "party", "GLM", "PamR", "nnet", "SVM", "xgboost")
    inn.exclude = alg_names[-which(Method == alg_names)]
  }
  if (hasArg(Kernel)){
    svm.kernel = Kernel
  } else {
    svm.kernel = "linear"
  }

  startTime <- Sys.time()

  labels <- ifelse(as.factor(labels) == levels(as.factor(labels))[1], 1, 2)
  class <- data.frame(y = (c(labels)))
  class.data <- cbind(data, class)
  actual.label <- data.frame(labels = class.data$y, row.names = rownames(class.data))

  #########################
  k = innerfolds
  nk = outerfolds


  if (hasArg(seed)){
    set.seed(seed)
  } else {
    seed = sample(1000, 1)
    set.seed(seed)
  }
  seed.list = sample(1000, nk)
  message("Seeds are: ", toString(seed.list))

  inner.feature.selection <- list()
  inner.blkbox <- list()
  inner.performance <- list()
  holdout.result <- list()
  holdout.result <- list()
  holdout.performance <- list()
  re.shuffle.counter <- 0

  while (!exists("nfold_intervals")){
    #set.seed(z)
    class.data <- class.data[sample(nrow(class.data)), ]
    #inital nested folds
    nfolds1 <- nrow(class.data) %% nk
    nsize1 <- floor(nrow(class.data) / nk + 1)
    nfolds2 <- nk - (nrow(class.data) %% nk)
    nsize2 <- floor(nrow(class.data) / nk)

    nfold_intervals = c(seq(from = 0, to = nfolds1 * nsize1, by = nsize1), seq(from = ( (nfolds1 * nsize1) + nsize2), to = (nfolds1 * nsize1 + nfolds2 * nsize2), by = nsize2))

    for (i in 1:nk){
      s1 <- nfold_intervals[i]+1
      s2 <- nfold_intervals[i+1]
      subset <- s1:s2
      levels.test <- class.data$y[subset]
      if (length(levels(as.factor(levels.test))) == 1){
        rm(nfold_intervals)
        re.shuffle.counter = re.shuffle.counter + 1
        break
      }
    }
  }
  message("Folds were reshufled ", re.shuffle.counter, " times.")
  if ( (nfolds1 * nsize1 + nfolds2 * nsize2) == nrow(class.data)){
    if (nfolds1 == 0){
      message(nk, " nested fold cross validation, fold size: ", nsize2)
    } else {
      message(nk, " nested fold cross validation with ", nfolds1, " folds of ", nsize1, " and ", nfolds2, " folds of ", nsize2)
    }
  } else {
    stop("error in cross validation, set folds parameter")
  }
  #########################

  ########

  #start loop - for the number of nested folds
  for (i in 1:nk){
    #splitting data
    s1 <- nfold_intervals[i] + 1
    s2 <- nfold_intervals[i + 1]
    subset <- s1:s2
    ncv.train <- class.data[-subset, ]
    ncv.test <- class.data[subset, ]
    nclasstr <- data.frame(condition = (factor(class.data$y[-subset])))
    nclassts <- data.frame(condition = (factor(class.data$y[subset])))

    #we take nk-1 data portion and give to blkbox functions
    #blkboxFS to reduce the data nk times)
    inner.feature.selection[[paste0("holdout_", i)]] <- blkboxCV(data = ncv.train[, -ncol(ncv.train)], labels = nclasstr$condition, folds = k, Method = Method, AUC = AUC, Gamma = svm.gamma, exclude = inn.exclude, seed = seed.list[i], max.depth = max.depth, xgtype = xgtype)
    #blkbox standard and performance to determine the inner loop performances
    inner.blkbox[[paste0("holdout_", i)]] <- blkboxCV(data = inner.feature.selection[[paste0("holdout_", i)]]$Feature_Selection$FS.data, labels = nclasstr$condition, folds = k, exclude = inn.exclude, seed = seed.list[i], max.depth = max.depth, xgtype = xgtype)
    inner.performance[[paste0("holdout_", i)]] <- Performance(inner.blkbox[[paste0("holdout_", i)]], metric = metric, consensus = TRUE)
    #Store results feature selected subset, their importance, the inner fold performance
    # return(list(data = inner.feature.selection[[paste0("holdout_", i)]]$Feature_Selection$FS.data,
    #             holdout = ncv.test,
    #             labels = nclasstr$condition,
    #             holdout.labels = nclassts$condition,
    #             seed = seed))
    #blkboxTrain on all BB_AFS$FS.data selected data and then blkboxPredict on ncv.test
    holdout.result[[paste0("holdout_", i)]] <- blkbox(data = inner.feature.selection[[paste0("holdout_", i)]]$Feature_Selection$FS.data,
                                                      holdout = ncv.test,
                                                      labels = nclasstr$condition,
                                                      holdout.labels = nclassts$condition,
                                                      Kernel = svm.kernel,
                                                      Gamma = svm.gamma,
                                                      mTry = m.try,
                                                      ntrees = nTrees,
                                                      exclude = exclude,
                                                      seed = seed.list[i],
                                                      max.depth = max.depth,
                                                      xgtype = xgtype)
    #performance of blkboxPredict result
    holdout.performance[[paste0("holdout_", i)]] <- Performance(holdout.result[[paste0("holdout_", i)]], metric = metric, consensus = TRUE)
  }

  #for each algorithm, make a table
  #table will be calculated by going through each

  temp_table1 <- list()
  temp_list1 <- list()
  for (alg in 1:length(names(holdout.result[[1]][[2]]))){
    for (i in 1:length(holdout.result)){
      if (i == 1){
        temp_table1[[names(holdout.result[[paste0("holdout_", i)]][[2]])[alg]]] <- data.frame(t(holdout.result[[paste0("holdout_",i)]][[2]][[alg]] * holdout.performance[[i]][[1]][[metric[1]]][[alg]]), row.names = paste("Holdout_", i))
      } else {
        temp_table1[[names(holdout.result[[paste0("holdout_", i)]][[2]])[alg]]] <- plyr::rbind.fill(temp_table1[[names(holdout.result[[paste0("holdout_", i)]][[2]])[alg]]], data.frame(t(holdout.result[[paste0("holdout_", i)]][[2]][[alg]] * holdout.performance[[paste0("holdout_", i)]][[1]][[metric[1]]][[alg]]), row.names = paste("Holdout_", i)))
      }
    }
    temp_table1[[names(holdout.result[[paste0("holdout_", i)]][[2]])[alg]]][is.na(temp_table1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]])] <- 0
    temp_list1[[names(holdout.result[[paste0("holdout_", i)]][[2]])[alg]]] <- data.frame(ImpWeightedAvg = sort(colMeans(temp_table1[[names(holdout.result[[paste0("holdout_",i)]][[2]])[alg]]]), decreasing = T))
  }

  merged_votes <- list()


  for (q in c(1:(9 - ifelse(exclude[1] == 0, 0, length(exclude))))){
    for (i in 1:nk){
      if (i == 1){
        merged_votes[[names(holdout.result[[paste0("holdout_", i)]]$algorithm.votes)[q]]] <- holdout.result[[paste0("holdout_",i)]]$algorithm.votes[[q]]
      } else {
        #bind
        merged_votes[[names(holdout.result[[paste0("holdout_", i)]]$algorithm.votes)[q]]] <- cbind(merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]], holdout.result[[paste0("holdout_",i)]]$algorithm.votes[[q]])
        if (i == nk){
          #order
          merged_votes[[names(holdout.result[[paste0("holdout_", i)]]$algorithm.votes)[q]]] <- t(as.matrix(merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]][, match(rownames(data), colnames(merged_votes[[names(holdout.result[[paste0("holdout_",i)]]$algorithm.votes)[q]]]))]))
        }
      }
    }
  }

  #performance
  ncv_votes <- list("algorithm.votes" = merged_votes, "input.data" = list("labels" = data.frame(labels)))
  ncv.perf <- blkbox::Performance(ncv_votes, metric = metric, consensus = F)

  ncv.perf <- lapply(seq_along(ncv.perf$Performance), function(x){
    df <- data.frame(unlist(ncv.perf$Performance[[x]]))
    colnames(df) <- names(ncv.perf$Performance[x])
    return(df)
  }) %>% Reduce(cbind, .)

  #return(holdout.performance)
  roc.data = lapply(seq_along(holdout.performance), function(x){
    df <- cbind(Rep = paste0("Holdout ", x), holdout.performance[[1]][3]$roc.values[,-1])
  })
  roc.data[["seperated"]] = Reduce(rbind, roc.data)
  roc.data[["combined"]] = ncv.perf$roc.values

  holdout.performance = lapply(seq_along(holdout.performance), function(x){
    df <- data.frame(temp_name = unlist(holdout.performance[[x]]$Performance))
    df <- cbind(df, t(data.frame(stringr::str_split(as.character(mapply(gsub, "[.]", "_", rownames(df))), "_", 2))),
               rep(names(holdout.performance[x]), length(unlist(holdout.performance[[x]]$Performance))))
    colnames(df) <- c("Performance", "Metric", "Algorithm", "Holdout")
    rownames(df) <- NULL
    return(df)
  }) %>%
    Reduce(rbind, .) %>%
    dplyr::select_("Algorithm", "Holdout", "Metric", "Performance") %>%
    dplyr::arrange_("Holdout")
  holdout.performance = data.frame(holdout.performance)

  list_tabs <- lapply(X = 1:length(temp_table1), y = temp_table1, function(X, y){
    names_c <- names(y)
    y <- data.frame(t(y[[X]]))
    colnames(y) <- paste0("Holdout_", 1:length(y))
    y <- tibble::rownames_to_column(y, var = "feature") %>%
      dplyr::mutate(algorithm = names_c[X]) %>%
      tidyr::gather_("Holdout", "Importance", c(paste0("Holdout_", c(1:outerfolds)))) %>%
      dplyr::mutate(Holdout = gsub("Holdout_", "", Holdout),
             Importance = Importance / max(Importance, na.rm = T))
    return(y)
  })

  list_tabs <- Reduce(rbind, list_tabs)
  list_tabs_summarised <- list_tabs %>%
    dplyr::group_by_("algorithm", "feature") %>%
    dplyr::summarise(Importance = mean(Importance, na.rm = T))

  endTime <- Sys.time()
  elapsedTime <- endTime - startTime

  #Output summary message
  cat("Overall Performance:\n")
  print(ncv.perf)



  #Output
  return(list("InnerFS" = inner.feature.selection,
              "InnerBB" = inner.blkbox,
              "InnerPerf" = inner.performance,
              "HoldoutRes" = holdout.result,
              "HoldoutPerf" = holdout.performance,
              "HoldoutPerfMerged" = ncv.perf,
              "FeatureTable" = list_tabs,
              "MeanFeatureTable" = list_tabs_summarised,
              "WeightedAverageImportance" = temp_list1,
              "roc.data" = roc.data,
              "benchmarks" = list("time" = elapsedTime)))

}

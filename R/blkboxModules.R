#RANDOM FOREST HIDDEN MODULE
.BB_RF <- function(cv.train, cv.test, classtr, m.try, nTrees, ...){

  test.rf = randomForest::randomForest(classtr$condition~., data = cv.train[,-ncol(cv.train)], mtry = m.try, ntree = nTrees, keep.forest = TRUE, importance = TRUE, test = cv.test)
  test.rf.pr = predict(test.rf, type = "prob", newdata = cv.test)[,2]
  rf_imp = data.frame(MeanDecreaseGini = randomForest::importance(test.rf)[,4])
  rf_imp = rf_imp[sort.list(rownames(rf_imp)), , drop = FALSE]

  return(list("VOTE" = t(data.frame(vote = test.rf.pr + 1)) , "IMP" = rf_imp))

}



#BIG RANDOM FOREST HIDDEN MODULE
.BB_BRF <- function(cv.train, cv.test, classtr, classts, m.try, nTrees){

  test.rf = bigrf::bigrfc(x = cv.train[,-ncol(cv.train)], y = classtr$condition, ntrees = nTrees, nsplitvar = m.try, cachepath = NULL)
  test.rf.pr = bigrf::predict(test.rf, cv.test[,-ncol(cv.test)], y = classts$condition)
  rf_imp = bigrf::fastimp(test.rf)
  rf_imp = as.data.frame(rf_imp)
  rf_imp = rf_imp[sort.list(rf_imp$rf_imp),, drop = FALSE]
  do.call(file.remove, list(paste0(tempdir(), "/", list.files(tempdir()))))

  return(list("VOTE" = t(data.frame(vote = test.rf.pr, row.names = row.names(cv.test))) , "IMP" = rf_imp))
}



#KKNN HIDDEN MODULE
.BB_KKNN <- function(cv.train, cv.test, distance, ...){


  test.kknn = kknn::kknn(y~.,cv.train, cv.test, distance=3)
  kknn_fit = stats::fitted(test.kknn)

  return(list("VOTE" = t(data.frame(vote = kknn_fit, row.names = row.names(cv.test)))))

}



#BARTMACHINE HIDDEN MODULE
.BB_BARTM <- function(cv.train, cv.test, nTrees, ...){

  BartM = bartMachine::build_bart_machine(X=NULL,y=NULL, Xy=cv.train, num_trees=nTrees, verbose = FALSE)
  BartMP = bartMachine::bart_predict_for_test_data(BartM, cv.test[,1:(ncol(cv.test)-1)], cv.test$y)
  if(length(grep(pattern = "Windows", x = Sys.info()[['sysname']])) == 0){
    { sink(file = "/dev/null");BartM_imp = bartMachine::investigate_var_importance(BartM); sink(); }
  } else {
    { sink("NUL"); BartM_imp = bartMachine::investigate_var_importance(BartM); sink(); }
  }
  BartM_imp2 = data.frame(AvgImp = BartM_imp$avg_var_props)

  return(list("VOTE" = t(data.frame(vote = BartMP$y_hat, row.names = row.names(cv.test))), "IMP" = BartM_imp2))

}


#PARTY HIDDEN MODULE
.BB_PARTY <- function(cv.train, cv.test, m.try, nTrees, ...){

  pd.cf = party::cforest(y ~., data = cv.train, controls = party::cforest_unbiased(ntree = nTrees, mtry = m.try))
  pd.cf_pred = predict(pd.cf, newdata = cv.test, OOB = TRUE)
  pd.cf_imp = data.frame(party_imp = abs(party::varimp(pd.cf)))

  return(list("VOTE" = t(data.frame(vote = pd.cf_pred[,1], row.names = row.names(cv.test))), "IMP" = pd.cf_imp))
}



#GLMNET HIDDEN MODULE
.BB_GLM <- function(cv.train, cv.test, ...){

  if(length(grep(pattern = "Windows", x = Sys.info()[['sysname']])) == 0){
    { sink(file = "/dev/null"); glm.tr = caret::train(x = cv.train[,-ncol(cv.train),drop=F], y = as.factor(cv.train$y), method = "glmnet"); sink(); }
  } else {
    { sink("NUL"); glm.tr = caret::train(x = cv.train[,-ncol(cv.train),drop=F], y = as.factor(cv.train$y), method = "glmnet"); sink(); }
  }
  glm.pr = predict(object = glm.tr, newdata = cv.test[,-ncol(cv.test), drop = F])
  glm.imp = caret::varImp(glm.tr)
  glm.imp = glm.imp$importance

  return(list("VOTE" = t(data.frame(votes = as.numeric(glm.pr), row.names = row.names(cv.test))) , "IMP" = glm.imp))

}



#PAMR HIDDEN MODULE
.BB_PAM <- function(cv.train, cv.test, ...){

  if(length(grep(pattern = "Windows", x = Sys.info()[['sysname']])) == 0){
    { sink(file = "/dev/null");pamr.tr = caret::train(x = cv.train[,-ncol(cv.train),drop=F], y = as.factor(cv.train$y), method = "pam"); sink(); }
  } else {
    { sink("NUL"); pamr.tr = caret::train(x = cv.train[,-ncol(cv.train),drop=F], y = as.factor(cv.train$y), method = "pam"); sink(); }
  }
  pamr.pr = predict(object = pamr.tr, newdata = cv.test[,-ncol(cv.test), drop = F])
  #pamr.imp = caret::varImp(object = pamr.tr, useModel = T)
  #pamr.imp = pamr.imp$importance$'1'
  pamr.threshold = pamr.tr$finalModel$tuneValue$threshold
  if(dim(pamr.tr$finalModel$centroids)[1] != dim(pamr.tr$finalModel$xData)[2]){
    stop("number of variables is not consistent with the pamr object")
  }
  pamr.imp.calc <- t(pamr.tr$finalModel$xData)
  centroids <- pamr::pamr.predict(pamr.tr$finalModel, pamr.imp.calc, threshold = pamr.threshold, type = "cent")
  standCentroids <- (centroids - pamr.tr$finalModel$centroid.overall)/pamr.tr$finalModel$sd
  standCentroids <- as.data.frame(standCentroids)
  standCentroids <- apply(standCentroids, 2, function(pamr.imp.calc) abs(pamr.imp.calc)*100/max(abs(standCentroids)))

  return(list("VOTE" = t(data.frame(vote = as.numeric(pamr.pr), row.names = row.names(cv.test))) , "IMP" = standCentroids[,1, drop = F]))

}


#NNET HIDDEN MODULE
.BB_NNET <- function(cv.train, cv.test, ...){

  if(length(grep(pattern = "Windows", x = Sys.info()[['sysname']])) == 0){
    { sink(file = "/dev/null");nnet.tr = caret::train(x = cv.train[,-ncol(cv.train),drop=F], y = as.factor(cv.train$y), method = "nnet", MaxNWts = 1000000); sink(); }
  } else {
    { sink("NUL"); nnet.tr = caret::train(x = cv.train[,-ncol(cv.train),drop=F], y = as.factor(cv.train$y), method = "nnet", MaxNWts = 1000000); sink(); }
  }
  nnet.pr = predict(object = nnet.tr, newdata = cv.test[,-ncol(cv.test), drop = F])
  nnet.imp = caret::varImp(nnet.tr)
  nnet.imp = nnet.imp$importance

  return(list("VOTE" = t(data.frame(votes = as.numeric(nnet.pr), row.names = rownames(cv.test))) , "IMP" = nnet.imp))

}


#SVM HIDDEN MODULE
.BB_SVM <- function(cv.train, cv.test, classtr, svm.kernel, svm.gamma, ...){


  svm.model = e1071::svm(classtr$condition ~ ., data = cv.train[,1:(ncol(cv.test)-1)] , kernel = svm.kernel, gamma = svm.gamma, probability = TRUE)
  svm.pred = predict(svm.model, cv.test[,1:(ncol(cv.test)-1), drop = F])
  if(svm.kernel == "linear"){
    svm.imp = t(data.frame((abs(t(svm.model$coefs) %*% svm.model$SV))^2))
    return(list("VOTE" =  t(data.frame(vote = as.numeric(svm.pred), row.names = row.names(cv.test))) , "IMP" = svm.imp))
  } else {
    return(list("VOTE" = t(data.frame(vote = as.numeric(svm.pred), row.names = row.names(cv.test)))))
  }
}

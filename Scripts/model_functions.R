# GLM Helper Functions ====

'
  GLM Functions to use in the report.
'

# Required Packages & Sources ====
# local functions
source("Scripts/utility_functions.R")

# packages
pkgs = c("dplyr", "ROCR")
invisible(package.check(pkgs))

# Functions ====

confmatrix <- function(probs, actual, db = NULL) {
  # creates conf matrix and a set of results about confmatrix
  # args: 
  #   preds: output of predict function
  #   actual: predictor dataset
  #   db: decision boundary value
  # returns:
  #   a list contains a conf matrix and a table of info.
  
  # rounding decimal point
  rd <- 4
  
  # confusion matrix
  if (!is.null(db)) {
    preds <- ifelse(probs > db, 1, 0)
  } else {
    preds <- probs
  }
  tab1 <- table(predicted = preds, actual = actual)
  
  # misclassification error
  mse <- 1 - sum(diag(tab1))/sum(tab1)
  tpr <- tab1[1,2] / tab1[2,2]
  fpr <- tab1[1,2] / tab1[1,1]
  
  # mean accuracy
  acc <- mean(preds == actual)
  
  # dummy guess ratio for actual data
  tab <- table(actual)
  all_true <- unname(tab[2] / sum(tab))
  all_fals <- unname(tab[1] / sum(tab))
  
  #printing
  df <- data.frame(tpr = tpr, fpr = fpr, mse = mse, accuracy = acc, if_all_false = all_fals, if_all_true = all_true)
  re <- list(matrix = tab1, information = df)
  sapply(re, round, rd)
}

roc_auc <- function(preds){
  # create accuracy and roc plot 
  # args: 
  #   preds: output of predict function
  
  # area under the curve value
  area_under_curve <-performance(preds, "auc")
  area_under_curve@y.values[[1]]
}

rocplot <- function(preds, actual, ...){
  # create accuracy and roc plot 
  # args: 
  #   preds: output of predict function
  #   actual: predictor dataset
  # returns:
  #   three plots
  predt = prediction(preds, actual)
  
  # accuracy acc to cutoff value
  eval = performance(predt, "acc")
  
  # true positive and false positive graph
  perf = performance(predt , "tpr", "fpr") 
  
  # plotting
  par(mfrow = c(1,3))
  hist(preds, prob = T, main="Predicted Probability Hist")
  lines(density(preds), col = "Red")
  plot(eval, col=rainbow(4), main="Accuracy Curve")
  plot(perf, col=rainbow(7), main="ROC Curve Admissions", ...)
  abline(0, 1)
  par(mfrow = c(1,1))
}

rocplot_compare <- function(train_predictions, test_predictions, train_actual, test_actual, ...){
  # compares roc plot btw train and test datasets
  # args: 
  #   preds: output of predict function
  #   actual: predictor dataset
  #   ... : graph paramaters
  # returns:
  #   two roc plots
  
  # train
  predt_tr = prediction(train_predictions, train_actual)
  perf_tr = performance(predt_tr, "tpr", "fpr")
  
  # train
  predt_te = prediction(test_predictions, test_actual)
  perf_te = performance(predt_te, "tpr", "fpr")
  
  #plotting
  par(mfrow = c(1,2))
  plot(perf_tr, col=rainbow(7), main="ROC - Train Data", ...)
  abline(0, 1)
  plot(perf_te, col=rainbow(7), main="ROC - Test Data", ...)
  abline(0, 1)
  par(mfrow = c(1,1))
}

cutoff_acc <- function(preds, actual){
  # cutoff values according to accuracy
  # args: 
  #   preds: output of predict function
  #   actual: predictor dataset
  # returns:
  #   matrix of acc and cutoff value
  predt = prediction(preds, actual)
  
  many.acc.perf = performance(predt, measure = "acc")
  
  mapply(function(x, y){
    ind = which.max( y )
    acc = y[ind]
    cutff = x[ind]
    return(c(accuracy= acc, cutof = cutff))
  }, slot(many.acc.perf, "x.values"), slot(many.acc.perf, "y.values"))
}

cutoff_roc <- function(preds, actual){
  # cutoff values according to roc
  # args: 
  #   preds: output of predict function
  #   actual: predictor dataset
  # returns:
  #   matrix of acc and cutoff value
  predt = prediction(preds, actual)
  
  # tpr and fpr spec and spef values
  perf = performance(predt , "tpr", "fpr")
  
  # func to find indices
  cut.ind = mapply(function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, predt@cutoffs)
  (cut.ind)
}

outlier_handler <- function(vctr){
  # removes outliers from given vector
  # args: 
  #   vctr: the vector that has outliers
  # returns:
  #   same vector without outliers
  
  x <- vctr
  qnt <-quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  h <- 1.5 * IQR(x)
  x[x < (qnt[1] - h)] <- caps[1]
  x[x > (qnt[2] + h)] <- caps[2]
  (vctr <- x)
}
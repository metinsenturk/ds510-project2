# GLM Helper Functions ====

'
  GLM Functions to use in the report.
'

# Required Packages & Sources ====
install.packages("dplyr")
install.packages("ROCR")
install.packages("popbio")
install.packages("aod")
install.packages("caret")

library(ROCR)
library(dplyr)
library(popbio)
library(aod)
library(caret)

source("Scripts/utility_functions.R")

# Functions ====

confmatrix <- function(probs, actual) {
  # rounding decimal point
  rd <- 4
  
  # confusion matrix
  preds <- ifelse(probs > 0.5, 1, 0)
  tab1 <- table(predicted = preds, actual = actual)
  
  # misclassification error
  mse <- round(1 - sum(diag(tab1))/sum(tab1), rd)
  tpr <- round(tab1[1,2] / tab1[2,2], rd)
  fpr <- round(tab1[1,2] / tab1[1,1], rd)
  
  # dummy guess ratio for actual data
  tab <- table(actual)
  all_true <- unname(round(tab[2] / sum(tab), rd))
  all_fals <- unname(round(tab[1] / sum(tab), rd))
  
  #printing
  df <- data.frame(tpr = tpr, fpr = fpr, mse = mse, all_false = all_fals, all_true = all_true)
  (list(mtrx = tab1, info = df))
}

roc_auc <- function(preds){
  # area under the curve value
  area_under_curve <-performance(preds, "auc")
  area_under_curve@y.values[[1]]
}

rocplot <- function(preds, actual, ...){
  predt = prediction(preds, actual)
  
  # accuracy acc to cutoff value
  eval = performance(predt, "acc")
  
  # true positive and false positive graph
  perf = performance(predt , "tpr", "fpr") 
  
  # plotting
  par(mfrow = c(1,2))
  plot(eval, col=rainbow(4), main="Accuracy Curve")
  plot(perf, col=rainbow(7), main="ROC Curve Admissions", ...)
  abline(0, 1)
  par(mfrow = c(1,1))
}

cutoff_acc <- function(preds, actual){
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
  predt = prediction(preds, actual)
  
  # tpr and fpr spec and spef values
  perf = performance(predt , "tpr", "fpr")
  
  # func to find indices
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, predt@cutoffs)
  (cut.ind)
}
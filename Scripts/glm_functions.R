# GLM Helper Functions ====

'
  GLM Functions to use in the report.
'

# Required Packages & Sources ====

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
  tab1
  
  # misclassification error
  mse <- round(1 - sum(diag(tab1))/sum(tab1), rd)
  tpr <- round(tab1[1,2] / tab1[2,2], rd)
  fpr <- round(tab1[1,2] / tab1[1,1], rd)
  
  # dummy guess ratio for actual data
  tab <- table(actual)
  tab
  all_true <- unname(round(tab[2] / sum(tab), rd))
  all_fals <- unname(round(tab[1] / sum(tab), rd))
  
  #printing
  df <- data.frame(tpr = tpr, fpr = fpr, mse = mse, all_false = all_fals, all_true = all_true)
  (list(mtrx = tab1, info = df))
}
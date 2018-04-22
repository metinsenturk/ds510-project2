# Support Vector Machines (SVM) Model ====

"
  Methodology for doing support vector machines.
"

# required pckgs and fnctns ====
# local sources
source("Scripts/utility_functions.R")
source("Scripts/model_functions.R")

# packages
pkgs = c("e1071")
invisible(package.check(pkgs))

# importing dataset ====
# for simplicity, we will check env if the variables we need are loaded. Otherwise, load first.
vars = c("data_raw", "data_train", "data_test", "f_0")
if (!exists(vars)) {
  stop("Variables in environment are missing.")
} else {
  head(data_raw)
  f_0
}

# variable analysis
plot(data_train$Age, y = data_train$AHD)

# svm model ====
svm_model <- svm(f_0, data = data_train, kernel = "radial", cost = 1, scale = F, type="C-classification") 
summary(svm_model)

plot(svm_model, data = data_train, AHD ~ Age)

preds <- predict(svm_model, data_train[,c(2, 5, 6, 9, 11)])



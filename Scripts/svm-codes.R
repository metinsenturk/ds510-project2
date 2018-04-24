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
  str(data_train)
}

# datasets updated with only cont type variables
data_train <- data_raw[, c(2, 5, 6, 9, 11, 15)]
data_test <- data_raw[, c(2, 5, 6, 9, 11, 15)]

# svm model ====
# model
svm_model <- svm(f_1, data = data_train, 
                 kernel = "radial", 
                 gamma = 1,
                 cost = 0.5,
                 decision.values=T,
                 scale = F, 
                 type = "C-classification") 
summary(svm_model)

# tuning
svm_tune <- tune(svm, f_2, data = data_raw, 
                 kernel = "radial", 
                 type = "C-classification",
                 decision.values = T,
                 scale = F,
                 tunecontrol = tune.control(cross = 10, nrepeat = 2),
                 tuneLength = 8,
                 ranges = list(gamma = 2^(-1:2), cost = 2^(-1:10)))
svm_tune$best.parameters
head(svm_tune$performances)

svm_model <- svm_tune$best.model

# conf matrix
preds <- predict(svm_model, data_train, type = "response")
confmatrix(preds, data_train$AHD)

# plotting variables individually
plot(data_train$Age, y = data_train$AHD, col = 2) 
par(new=T)
plot(data_train[,2], svm_model$fitted, col = 3)

# roc
preds <- predict(svm_model, data_train, decision.values = T)
attrs <- attributes(preds)$decision.values
rocplot(attrs, data_train$AHD)


# applying model to test data
# roc for test
preds <- predict(svm_model, data_test, decision.values = T)
attrs <- attributes(preds)$decision.values
rocplot(attrs, data_test$AHD)

# conf matrix
preds <- predict(svm_model, data_test)
confmatrix(preds, data_test$AHD)



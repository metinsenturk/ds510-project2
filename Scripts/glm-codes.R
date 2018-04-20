# Logistic Regression Model ====

"
  Methodology for doing lgm.
"
install.packages("dplyr")
install.packages("ROCR")
library(ROCR)
library(dplyr)

# importing dataset
data_raw = read.csv("./Dataset/Heart.csv")
head(data_raw)

# Data Examining ====
# examining variables
str(data_raw)

# variables needs to be converted to factor <- Sex, Fbs, RestECG, ExAng, Ca, Slope
data_raw$Sex = as.factor(data_raw$Sex)
data_raw$Fbs = as.factor(data_raw$Fbs)
data_raw$RestECG = as.factor(data_raw$RestECG)
data_raw$ExAng = as.factor(data_raw$ExAng)
data_raw$Ca = as.factor(data_raw$Ca)
data_raw$Slope = as.factor(data_raw$Slope)

# examining factor levels
sapply(list(data_raw$Sex, 
            data_raw$Fbs, 
            data_raw$RestECG, 
            data_raw$Ca, 
            data_raw$Slope,
            data_raw$ChestPain,
            data_raw$Thal), levels)

# examining NA values
summarise_if(data_raw, is.atomic, funs(sum(is.na(.))))
filter((data_raw), is.na(Ca) | is.na(Thal))
# Thal has 2 missing values: replacing with most frequent level: normal
data_raw$Thal[is.na(data_raw$Thal)] = levels(data_raw$Thal)[2]
# Ca has 4 missing values: replacing with most frequent level: 0
data_raw$Ca[is.na(data_raw$Ca)] = levels(data_raw$Ca)[1]

# for reproducable results, seed. TODO: we need to change this to kfold
set.seed(1000)

# creating dataset for train and test
ind = sample(x = 2, size = nrow(data_raw), replace = T, prob = c(0.9, 0.1))
data_train = data_raw[ind == 1, ]
data_test = data_raw[ind == 2, ]

# frequency tables of AHD and factorial variables
tab1 = ftable(xtabs(~ AHD + Sex, data = data_train)) 
tab2 = ftable(xtabs(~ AHD + Fbs, data = data_train))
tab3 = ftable(xtabs(~ AHD + RestECG, data = data_train))
tab4 = ftable(xtabs(~ AHD + ExAng, data = data_train))
tab5 = ftable(xtabs(~ AHD + Ca, data = data_train))
tab6 = ftable(xtabs(~ AHD + Slope, data = data_train))
tab7 = ftable(xtabs(~ AHD + ChestPain, data = data_train))
tab8 = ftable(xtabs(~ AHD + Thal, data = data_train))

# running chisq test to understand correlation btw pairs. The smaller the better in terms of relationship. tab2 found to be high in p.
lapply(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8), chisq.test)
chisq.test(tab2)
round(prop.table(tab2), 2)
tab2

# Models ====
# logistic regression model
lgm_model = glm(AHD ~ ., data = data_train, family = binomial)
summary(lgm_model)

# goodness-of-fit test
with(lgm_model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F))

# train results ====
# the case of assuming all are healthy
tab <- table(data_train$AHD)
tab
unname(round(tab[2] / sum(tab), 4))

# prediction
p_tr_probs = predict(lgm_model, data_train, type = "response")
head(p_tr_probs)
head(data_train)

# misclassification error and confusion matrix
p_tr_preds <- ifelse(p_tr_probs > 0.5, 1, 0)
tab_tr_cm <- table(predicted = p_tr_preds, actual = data_train$AHD)
tab_tr_cm
mse <- 1 - sum(diag(tab_tr_cm))/sum(tab_tr_cm)
mse
tpr <- tab_tr_cm[1,2] / tab_tr_cm[2,2]
tpr
fpr <- tab_tr_cm[1,2] / tab_tr_cm[1,1]
fpr

# test results ====
# the case of assuming all are healthy
tab <- table(data_test$AHD)
tab
unname(round(tab[2] / sum(tab), 4))

# prediction
p_te_probs = predict(lgm_model, data_test, type = "response")
head(p_te_probs)
head(data_train)

# misclassification error and confusion matrix
p_te_preds <- ifelse(p_te_probs > 0.5, 1, 0)
tab_te_cm <- table(predicted = p_te_preds, actual = data_test$AHD)
tab_te_cm
mse <- 1 - sum(diag(tab_te_cm))/sum(tab_te_cm)
mse
tpr <- tab_te_cm[1,2] / tab_te_cm[2,2]
tpr
fpr <- tab_te_cm[1,2] / tab_te_cm[1,1]
fpr

# evaluation of model according to cutoff value ====
# probabilities frequency distribution
probs <- predict(lgm_model, data_train, type = "response")
head(probs)
hist(probs)

# accuracy acc to cutoff value
probs <- prediction(probs, data_train$AHD)
evals <- performance(probs, "acc")
plot(evals)

# true positive and false positive graph
true_positive_rate <- performance(probs, "tpr", "fpr")
plot(true_positive_rate, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")
abline(0, 1)

# area under the curve value
area_under_curve <-performance(probs, "auc")
area_under_curve@y.values[[1]]

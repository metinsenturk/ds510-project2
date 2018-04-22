# Logistic Regression Model ====

"
  Methodology for doing lgm.
"

# required pckgs and fnctns ====
# local sources
source("Scripts/utility_functions.R")
source("Scripts/model_functions.R")

# packages
install.packages("dplyr")
install.packages("popbio")
install.packages("aod")
install.packages("caret")
install.packages("leaps")
install.packages("bestglm")
install.packages("boot")

library(MASS)
library(dplyr)
library(popbio)
library(aod)
library(caret)
library(leaps)
library(bestglm)
library(boot)


# importing dataset
data_raw = read.csv("./Dataset/Heart.csv")
head(data_raw)

# Data Examining ====
# examining variables
str(data_raw)

# assigning 0 and 1 to predictor
data_raw$AHD = ifelse(data_raw$AHD == "Yes", 1, 0)

# variables needs to be converted to factor <- Sex, Fbs, RestECG, ExAng, Ca, Slope
data_raw$Sex = as.factor(data_raw$Sex)
data_raw$Fbs = as.factor(data_raw$Fbs)
data_raw$RestECG = as.factor(data_raw$RestECG)
data_raw$ExAng = as.factor(data_raw$ExAng)
data_raw$Ca = as.factor(data_raw$Ca)
data_raw$Slope = as.factor(data_raw$Slope)

# examining factor levels
sapply(data_raw, function(x) {length(unique(x))})
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

# normalization
data_raw %>%
  select(-one_of(c("X"))) %>%
  select_if(is.numeric)  %>%
  head
# continuous variables
cont_list = list(
  Age = data_raw$Age,
  RestBP = data_raw$RestBP,
  Chol = data_raw$Chol,
  MaxHR = data_raw$MaxHR,
  Oldpeak = data_raw$Oldpeak
)
# examining and changing variables
head(sapply(cont_list, scale))
data_raw[, names(cont_list)] <- data.frame(sapply(cont_list, scale))
head(data_raw)

#dealing with outliers
boxplot(data_raw[,c(2, 5, 6, 9, 11)], 
        log = "x", 
        las = 2,
        boxwex = 0.3)

data_raw$RestBP = outlier_handler(data_raw$RestBP)
data_raw$Chol = outlier_handler(data_raw$Chol)
data_raw$Oldpeak = outlier_handler(data_raw$Oldpeak)

# creating dataset for train and test
# other way to split dataset
set.seed(1000)
ind = sample(x = 2, size = nrow(data_raw), replace = T, prob = c(0.75, 0.2))
data_train = data_raw[ind == 1, ]
data_test = data_raw[ind == 2, ]

# train and test datasets
data_train = data_raw[ 1:250, ]
data_test = data_raw[251:303, ]

# variable analysis ====
#some ratios
#overall healthy rate
sum(data_train$AHD[data_train$AHD == 1]) / length(data_train$AHD)

# correlation of cont variables
cor(data.frame(cont_list))
cor(data_raw$AHD, data.frame(cont_list))

# frequency tables of AHD and factorial variables
tab1 = ftable(xtabs(~ AHD + Sex, data = data_train)) 
tab2 = ftable(xtabs(~ AHD + Fbs, data = data_train))
tab3 = ftable(xtabs(~ AHD + RestECG, data = data_train))
tab4 = ftable(xtabs(~ AHD + ExAng, data = data_train))
tab5 = ftable(xtabs(~ AHD + Ca, data = data_train))
tab6 = ftable(xtabs(~ AHD + Slope, data = data_train))
tab7 = ftable(xtabs(~ AHD + ChestPain, data = data_train))
tab8 = ftable(xtabs(~ AHD + Thal, data = data_train))

# running chisq test to understand correlation btw pairs. The smaller the better in terms of relationship. tab2 
# found to be high in p.
lapply(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8), chisq.test)
chisq.test(tab2)
round(prop.table(tab2), 2)
tab2

# Models ====
# logistic regression model
f_0 <- AHD ~ Age + RestBP + Chol + MaxHR + Oldpeak
f_1 <- AHD ~ MaxHR + Oldpeak + RestBP
f_2 <- AHD ~ RestBP + Chol + MaxHR + Oldpeak

f_9 <- AHD ~ . -X

lgm_model = glm(f_0, data = data_train, family = binomial)
summary(lgm_model)

# odds ratio
cbind(exp(confint(lgm_model)), Ods_Ratio = exp(coef(lgm_model)), Coef = coef(lgm_model))

# Anova
anova(lgm_model)

# stepAIC
steps <- stepAIC(lgm_model, trace = T)
steps
summary(steps)

steps <- step(lgm_model)
summary(steps)

# leaps
regsubs <- regsubsets(AHD ~ Age + RestBP + Chol + MaxHR + Oldpeak, 
           data = data_train,
           nbest = 1,
           method = "exhaustive")
regsubs
summary(regsubs)

# bestglm
plot_list$AHD <- data_train$AHD

best_glm <- bestglm(data.frame(plot_list),
                    family = binomial,
                    IC = "AIC")
best_glm$BestModels

# wald test
wald.test(b = coef(lgm_model), Sigma = vcov(lgm_model), Terms = 2:4)

# goodness-of-fit test
with(lgm_model, 
     pchisq(null.deviance - deviance, 
            df.null - df.residual, 
            lower.tail = F))

# evaluation of model according to cutoff value ====
probs <- predict(lgm_model, data_train, type = "response")
rocplot(probs, data_train$AHD)
cutoff_acc(probs, data_train$AHD)
cutoff_roc(probs, data_train$AHD)

# conf matrix test
db <- unname(cutoff_acc(probs, data_train$AHD)[2,1])
p_tr_probs = predict(lgm_model, data_train, type = "response")
head(p_tr_probs)
head(data_train)
cfi_tr <- confmatrix(p_tr_probs, data_train$AHD, db)
cfi_tr$mtrx
cfi_tr$info

p_te_probs = predict(lgm_model, data_test, type = "response")
head(p_te_probs)
head(data_train)
cfi_te <- confmatrix(p_te_probs, data_test$AHD, db)
cfi_te$mtrx
cfi_te$info

# plots about the data ====
# cont type variables in train dataset
plot_list = list(
  Age = data_train$Age,
  RestBP = data_train$RestBP,
  Chol = data_train$Chol,
  MaxHR = data_train$MaxHR,
  Oldpeak = data_train$Oldpeak
)

# plotting individually
par(mfrow = c(2,3))
sapply(plot_list, plot, y=data_train$AHD)
par(mfrow = c(1,1))

#another graph
logi.hist.plot(probs, data_train$AHD, boxp = F, type = "count", col = "gray", xlabel = "Age")

# all cont type variables
pairs(AHD ~ Age + RestBP + Chol + MaxHR + Oldpeak, 
      data = data_train,
      diag.panel = panel.hist,
      lower.panel = panel.cor) 

#kfold
trcontrol <- trainControl(method = "cv", number = 10)
trcontrol
train(f.all, data_train, method = 'glm', family = binomial, trcontrol = trcontrol)

cv_model <- cv.glm(data_train, lgm_model)
cv_model$delta
cv_model <- cv.glm(data_train, lgm_model, K = 10)
cv_model$delta
flds <- createFolds(data_train, k = 10, list = TRUE, returnTrain = FALSE)
flds

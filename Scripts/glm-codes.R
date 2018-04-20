# Logistic Regression Model

install.packages("dplyr")

library(dplyr)

# importing dataset
data_raw = read.csv("./Dataset/Heart.csv")
head(data_raw)

# examining variables
str(data_raw)

# variables needs to be converted to factor <- Sex, Fbs, RestECG, ExAng, Ca, Slope
data_raw$Sex = as.factor(data_raw$Sex)
data_raw$Fbs = as.factor(data_raw$Fbs)
data_raw$RestECG = as.factor(data_raw$RestECG)
data_raw$ExAng = as.factor(data_raw$ExAng)
data_raw$Ca = as.factor(data_raw$Ca)
data_raw$Slope = as.factor(data_raw$Slope)

# examining NA values
summarise_if(data_raw, is.atomic, funs(sum(is.na(.))))
filter((data_raw), is.na(Ca) | is.na(Thal))

# frequency tables of AHD and factorial variables
tab1 = ftable(xtabs(~ AHD + Sex, data = data_raw)) 
tab2 = ftable(xtabs(~ AHD + Fbs, data = data_raw))
tab3 = ftable(xtabs(~ AHD + RestECG, data = data_raw))
tab4 = ftable(xtabs(~ AHD + ExAng, data = data_raw))
tab5 = ftable(xtabs(~ AHD + Ca, data = data_raw))
tab6 = ftable(xtabs(~ AHD + Slope, data = data_raw))

# running chisq test to understand correlation btw pairs. The smaller the better in terms of relationship. tab2 found to be high in p.
lapply(list(tab1,tab2,tab3,tab4,tab5,tab6), chisq.test)
round(prop.table(tab2), 2)
chisq.test(tab2)
tab2

# examining factor levels
sapply(list(data_raw$Sex, data_raw$Fbs, data_raw$RestECG, data_raw$Ca, data_raw$Slope), levels)








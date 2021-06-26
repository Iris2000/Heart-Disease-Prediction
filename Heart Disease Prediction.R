# import dataset

data <- read.csv("C:\\Users\\lvlip\\Documents\\BCSI Sem 4\\IBM 3201\\Assignment\\heart_disease_uci.csv")

# attribute selection - exclude "id" and "dataset"

library(dplyr)

heart <- select(data, c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalch",
                        "exang", "oldpeak", "slope", "ca", "thal", "num"))

# check the structure and summary

str(heart)
summary(heart)

head(heart)
tail(heart)

# change character type and logical type to factor
# define levels

heart$sex <- as.factor(heart$sex)
levels(heart$sex)
levels(heart$sex) <- c("Female", "Male")

heart$cp <- as.factor(heart$cp)
levels(heart$cp)
levels(heart$cp) <- c("Asymptomatic", "Atypical Angina", "Non-Anginal", "Typical Angina")

heart$fbs <- as.factor(heart$fbs)
levels(heart$fbs) <- c("False", "True")

heart$restecg <- as.factor(heart$restecg)
levels(heart$restecg)
levels(heart$restecg) <- c("NA", "Lv Hypertrophy", "Normal", "St-t Abnormality")

heart$exang <- as.factor(heart$exang)
levels(heart$exang) <- c("False", "True")

heart$slope <- as.factor(heart$slope)
levels(heart$slope)
levels(heart$slope) <- c("NA", "Downsloping", "Flat", "Upsloping")

heart$ca <- as.factor(heart$ca)

heart$thal <- as.factor(heart$thal)
levels(heart$thal)
levels(heart$thal) <- c("NA", "Fixed Defect", "Normal", "Reversable Defect")

heart$num <- as.factor(heart$num)

# set level to NA

levels(heart$restecg)[levels(heart$restecg)=='NA'] <- NA
levels(heart$slope)[levels(heart$slope)=='NA'] <- NA
levels(heart$thal)[levels(heart$thal)=='NA'] <- NA

summary(heart)

# check the outliers

out_trestbps = boxplot(heart$trestbps)$out
out_trestbps

out_chol = boxplot(heart$chol)$out
out_chol

out_thalch = boxplot(heart$thalch)$out
out_thalch
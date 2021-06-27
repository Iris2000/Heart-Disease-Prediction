library(dplyr)
library(ggplot2)

# import dataset

data <- read.csv("C:\\Users\\lvlip\\Documents\\BCSI Sem 4\\IBM 3201\\Assignment\\heart_disease_uci.csv")

# attribute selection - exclude "id" and "dataset"

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

# change presence level 0 1 2 3 4 to 0 and 1 only

heart[which(heart$num %in% c(1,2,3,4)), "num"] <- 1
heart$num <- as.factor(heart$num)

# set level to NA

levels(heart$restecg)[levels(heart$restecg)=='NA'] <- NA
levels(heart$slope)[levels(heart$slope)=='NA'] <- NA
levels(heart$thal)[levels(heart$thal)=='NA'] <- NA

summary(heart)

# CHECK THE OUTLIERS
# outliers of trestbps

out_trestbps = boxplot(heart$trestbps)$out
out_trestbps

ggplot(data = heart, mapping = aes(x = trestbps)) +
  geom_histogram(bins = 100, fill = "blue") + 
  labs(x = "trestbps") +
  ggtitle("historgram of tresbps") +
  theme(plot.title = element_text(hjust = 0.5))

# the presence of heart disease with trestbps outliers

heart[which(heart$trestbps %in% out_trestbps), c('trestbps', 'num')]

# outliers of chol

out_chol = boxplot(heart$chol)$out
out_chol

# change chol 0 to NA due to too many 0 detected

heart$chol[heart$chol == 0] <- NA
summary(heart)

# check the chol outliers again

out_chol = boxplot(heart$chol)$out
out_chol

# outliers of thalch

out_thalch = boxplot(heart$thalch)$out
out_thalch

# DEAL WITH OUTLIERS
# replace outliers with q1 and q3

# replace outliers of trestbps

trestbpsq1 <- quantile(heart$trestbps, probs=0.25, na.rm=TRUE)
trestbpsq1

trestbpsmin <- trestbpsq1 - 1.5 * IQR(heart$trestbps, na.rm=TRUE)
trestbpsmin

trestbpsq3 <- quantile(heart$trestbps, probs=0.75, na.rm=TRUE)
trestbpsq3

trestbpsmax <- trestbpsq3 + 1.5 * IQR(heart$trestbps, na.rm=TRUE)
trestbpsmax

heart$trestbps = ifelse(heart$trestbps > trestbpsmax, trestbpsq3, heart$trestbps)
heart$trestbps = ifelse(heart$trestbps < trestbpsmin, trestbpsq1, heart$trestbps)

boxplot(heart$trestbps)

# replace outliers of chol

cholq1 <- quantile(heart$chol, probs=0.25, na.rm=TRUE)
cholq1

cholmin <- cholq1 - 1.5 * IQR(heart$chol, na.rm=TRUE)
cholmin

cholq3 <- quantile(heart$chol, probs=0.75, na.rm=TRUE)
cholq3

cholmax <- cholq3 + 1.5 * IQR(heart$chol, na.rm=TRUE)
cholmax

heart$chol = ifelse(heart$chol > cholmax, cholq3, heart$chol)
heart$chol = ifelse(heart$chol < cholmin, cholq1, heart$chol)

boxplot(heart$chol)

# replace outliers of thalch

thalchq1 <- quantile(heart$thalch, probs=0.25, na.rm=TRUE)
thalchq1

thalchmin <- thalchq1 - 1.5 * IQR(heart$thalch, na.rm=TRUE)
thalchmin

heart$thalch = ifelse(heart$thalch < thalchmin, thalchq1, heart$thalch)

boxplot(heart$thalch)
summary(heart)

options(max.print = 1e6)
mydata<-read.csv("D:/Fall 2023-24 11th sem/Introduction to Data Science/Dataset_Midterm.csv", header = TRUE, sep = ",")
mydata

names(mydata)

library(dplyr)

mydata <- mydata %>% mutate(gender = ifelse(grepl("^\\s*$", gender), NA, gender))
missingdata <- which(is.na(mydata$gender))



mydata <- mydata %>% mutate(smoking_history = ifelse(grepl("^\\s*$", smoking_history), NA, smoking_history))
missingdata_sh <- which(is.na(mydata$smoking_history))
mydata$smoking_history[mydata$smoking_history == "No Info"] <- NA




is.na(mydata)
mydata



which(is.na(mydata$gender))
which(is.na(mydata$age))
which(is.na(mydata$hypertension))
which(is.na(mydata$smoking_history))




mydata <- mydata[!is.na(as.numeric(mydata$age)), ]
unique(mydata$age)
mydata$age[mydata$age < 0] <- abs(mydata$age[mydata$age < 0])


mydata <- mydata[!is.na(as.numeric(mydata$bmi)), ]
unique(mydata$bmi)
mydata$bmi[mydata$bmi < 0] <- abs(mydata$bmi[mydata$bmi < 0])



par(mfrow = c(2, 2))
boxplot(mydata$age, main = "Age")
boxplot(mydata$bmi, main = "BMI")
boxplot(mydata$HbA1c_level, main = "HbA1c Level")
boxplot(mydata$blood_glucose_level, main = "Blood Glucose Level")



mydata_mean <- mydata
mean_age<- mean(mydata_mean$age, na.rm = TRUE)
mydata_mean$age[is.na(mydata_mean$age)] <- mean_age

print(mydata_mean)




missing_count <- colSums(is.na(mydata))
missing_count



library(ggplot2)
mydata_remove <-mydata
mydata_remove <- mydata_remove[complete.cases(mydata_remove), ]
mydata_remove

ggplot(mydata_remove, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Age Distribution")


missing_count <- colSums(is.na(mydata))
missing_count





mydata_mode <- mydata

mode_gender <- names(sort(table(mydata_mode$gender), decreasing = TRUE))[1]
mydata_mode$gender[is.na(mydata_mode$gender)] <- mode_gender

mode_age <- names(sort(table(mydata_mode$age), decreasing = TRUE))[1]
mydata_mode$age[is.na(mydata_mode$age)] <- mode_age

mode_hypertension <- names(sort(table(mydata_mode$hypertension), decreasing = TRUE))[1]
mydata_mode$hypertension[is.na(mydata_mode$hypertension)] <- mode_hypertension

mode_hsmoking <- names(sort(table(mydata_mode$smoking_history), decreasing = TRUE))[1]
mydata_mode$smoking_history[is.na(mydata_mode$smoking_history)] <- mode_hsmoking

mydata_mode




mydata_median <- mydata
median(mydata_median$age)
mydata_median


missing_count <- colSums(is.na(mydata))
missing_count


install.packages("ggplot2")
library(ggplot2)


ggplot(mydata, aes(x = hypertension)) +
  geom_bar() +
  labs(x = "Hypertension", y = "Count", title = "Distribution of Hypertension Categories")

ggplot(mydata, aes(x = heart_disease)) +
  geom_bar() +
  labs(x = "Heart Disease", y = "Count", title = "Distribution of Heart Disease Categories")

ggplot(mydata, aes(x = smoking_history)) +
  geom_bar() +
  labs(x = "Smoking History", y = "Count", title = "Distribution of Smoking History Categories")

ggplot(mydata, aes(x = diabetes)) +
  geom_bar() +
  labs(x = "Diabetes", y = "Count", title = "Distribution of Diabetes Categories")








ggplot(mydata_remove, aes(x = hypertension)) +
  geom_bar() +
  labs(x = "Hypertension", y = "Count", title = "Distribution of Hypertension Categories")

ggplot(mydata_remove, aes(x = heart_disease)) +
  geom_bar() +
  labs(x = "Heart Disease", y = "Count", title = "Distribution of Heart Disease Categories")

ggplot(mydata_remove, aes(x = smoking_history)) +
  geom_bar() +
  labs(x = "Smoking History", y = "Count", title = "Distribution of Smoking History Categories")

ggplot(mydata_remove, aes(x = diabetes)) +
  geom_bar() +
  labs(x = "Diabetes", y = "Count", title = "Distribution of Diabetes Categories")







ggplot(mydata, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Age Distribution")

ggplot(mydata, aes(x = bmi)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "BMI", y = "Frequency", title = "BMI Distribution")

ggplot(mydata, aes(x = HbA1c_level)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "HbA1c level", y = "Frequency", title = "HbA1x Level Distribution")

ggplot(mydata, aes(x = blood_glucose_level)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Blood Glucose Level", y = "Frequency", title = "Blood Glucose Level Distribution")




ggplot(mydata_remove, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Age Distribution")

ggplot(mydata_remove, aes(x = bmi)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(x = "BMI", y = "Frequency", title = "BMI Distribution")

ggplot(mydata_remove, aes(x = HbA1c_level)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(x = "HbA1c level", y = "Frequency", title = "HbA1x Level Distribution")

ggplot(mydata_remove, aes(x = blood_glucose_level)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(x = "Blood Glucose Level", y = "Frequency", title = "Blood Glucose Level Distribution")


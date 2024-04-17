# loading the packages needed for the assignment/tasks
library(dplyr)
library(tidyr)
library(skimr) # for summary statistics table
library(scales)
library(ggplot2)
library(corrplot)
library(rpart)
library(caret) # confusion matrix
library(tree) # for Decision Tree
library(e1071) # for Naive Bayes and Support Vector Machines
library(randomForest) # for Random Forest
library(adabag) # for Bagging and Boosting
library(neuralnet) # for ANN
library(ROCR) # for AUC and ROC
library(pROC)

library(openxlsx)
library(svDialogs)
library(data.table)
library(caret)
library(dplyr)
library(purrr)
library(doMC)
library(ROCit)

# reading the csv file required and creating individual data by setting a seed (my Student ID)
df <- read.csv("Lymph_dataset_raw.csv")

Table1 <- df %>%
  select(-c("id", "opd", "nam.y", "lnn","int", "le"))
#select(-c("id", "opd", "nam.y", "tax", "lnn","axi","int", "che", "fx", "Gy", "recon", "le"))

Table1$Endpoint <- factor(df$le)

randomseed <- 1165# 365# 1675# 

set.seed(randomseed)

### SVM ###
split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

svm.train <- svm(Endpoint~., data = train_data, kernel = 'radial')

# Predict the binary response on the test data
svm_predictions <- predict(svm.train, test_data)

# Calculate accuracy for the SVM model
svm_accuracy <- mean(svm_predictions == test_data$Endpoint)

# Print the SVM accuracy
cat("SVM Accuracy:", svm_accuracy, "\n")

# AUC, sensitivity and specificity
confusionMatrix(table(actual = test_data$Endpoint, predicted = svm_predictions))

svm.test <- predict(svm.train, test_data)
svm.pred <- prediction(as.numeric(svm.test), test_data$Endpoint)
svm.perf <- performance(svm.pred, "tpr", "fpr")
svm.auc <- performance(svm.pred, "auc")
cat("AUC for Support Vector Machines: ", as.numeric(svm.auc@y.values))


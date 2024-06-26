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
library(C50) # For C5.0
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
df <- read.csv("Lymph_dataset.csv")

Table1 <- df %>%
  select(-c("le"))

Table1$Endpoint <- factor(df$le)

# Exclude the Endpoint variable before normalizing
independent_variables <- setdiff(names(Table1), "Endpoint")

# Normalize independent variables
Table1[, independent_variables] <- scale(Table1[, independent_variables])

randomseed <- 1165# 365# 1675# 

set.seed(randomseed)

### Decision Tree ###
split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

C5.model <- C5.0(Endpoint~., data = train_data)

# Predict the binary response on the test data
C5_predictions <- predict(C5.model, test_data, type="class")

# Calculate accuracy
C5_accuracy <- mean(C5_predictions == test_data$Endpoint)

# Print the accuracy
cat("C5.0 Accuracy:", C5_accuracy, "\n")

# AUC, sensitivity and specificity
performance <- confusionMatrix(C5_predictions, test_data$Endpoint, positive = "1")
performance
performance$byClass["F1"]

C5_predictions_prob <- predict(C5.model, newdata = test_data, type="prob")[,2]
ROCit_obj_test <- rocit(score=C5_predictions_prob, class=test_data$Endpoint)
ROCit_obj_test$AUC

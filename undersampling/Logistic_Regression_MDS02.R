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
library(ROSE) # for undersampling

# reading the csv file required and creating individual data by setting a seed (my Student ID)
#df <- read.csv("lymphedema_dataset.csv")
DataTable <- read.csv("Lymph_dataset.csv")

#-------------------------------------------------------------------------------
# Select variables + endpoint
Table1 <- DataTable %>%
  select(-c("id", "opd", "nam.y", "lnn","int", "le"))
#select(-c("id", "opd", "nam.y", "tax", "lnn","axi","int", "che", "fx", "Gy", "recon", "le"))

Table1$Endpoint <- factor(DataTable$le)

# Exclude the Endpoint variable before normalizing
independent_variables <- setdiff(names(Table1), "Endpoint")

# Normalize independent variables
Table1[, independent_variables] <- scale(Table1[, independent_variables])

randomseed <- 1165# 365# 1675#

set.seed(randomseed)

# Split the data into training and testing sets (e.g., 80% training and 20% testing)
split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

undersample_train_data <- ovun.sample(Endpoint~., data=train_data, p=0.5, seed=1165, method="under")$data
table(undersample_train_data$Endpoint)

# Fit the logistic regression model on the training data
lr.model <- glm(Endpoint~., family = "binomial", data = undersample_train_data)
# Predict the binary response on the test data
lr_predictions <- predict(lr.model, newdata = test_data, type = "response")
# Convert predictions to binary (0 or 1)
lr_predictions_binary <- ifelse(lr_predictions > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(lr_predictions_binary == test_data$Endpoint)

# Print the accuracy
cat("Accuracy:", accuracy, "\n")

# AUC, sensitivity and specificity
performance <- confusionMatrix(factor(lr_predictions_binary), test_data$Endpoint, positive = "1")
performance
performance$byClass["F1"]

ROCit_obj_test <- rocit(score=lr_predictions,class=test_data$Endpoint)
ROCit_obj_test$AUC

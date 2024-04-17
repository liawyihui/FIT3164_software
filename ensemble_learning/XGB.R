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
library(caretEnsemble)
library(xgboost)

# reading the csv file required and creating individual data by setting a seed (my Student ID)
df <- read.csv("Lymph_dataset_raw.csv")

Table1 <- df %>%
  select(-c("id", "opd", "nam.y", "lnn","int", "le"))
#select(-c("id", "opd", "nam.y", "tax", "lnn","axi","int", "che", "fx", "Gy", "recon", "le"))

Table1$Endpoint <- factor(df$le)

# Exclude the Endpoint variable before normalizing
independent_variables <- setdiff(names(Table1), "Endpoint")

Table1[, independent_variables] <- scale(Table1[, independent_variables])

randomseed <- 1165# 365# 1675# 

set.seed(randomseed)

split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

# Convert labels to numeric (0 and 1) for XGBoost
train_data$Endpoint <- as.numeric(train_data$Endpoint) - 1
test_data$Endpoint <- as.numeric(test_data$Endpoint) - 1

# Convert data to DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, independent_variables]), label = train_data$Endpoint)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, independent_variables]), label = test_data$Endpoint)

# Train XGBoost model
model <- xgboost(
  data = dtrain, 
  eta = 1,                 # Learning rate
  nthread = 2,             # Number of threads to use for parallelism
  nrounds = 100,             # Number of boosting rounds
  objective = "binary:logistic",  # Objective function
)

# Make predictions on test data
predictions <- predict(model, dtest, type="prob")
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Evaluate model performance
accuracy <- mean(binary_predictions == test_data$Endpoint)
cat("Accuracy:", accuracy, "\n")

performance <- confusionMatrix(factor(binary_predictions), factor(test_data$Endpoint), positive = "1")
performance
performance$byClass["F1"]

ROCit_obj_test <- rocit(score=predictions, class=test_data$Endpoint)
ROCit_obj_test$AUC
setwd("D:/FIT3164_software/test")
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

# reading the csv file required and creating individual data by setting a seed (my Student ID)
rm(list = ls())
df <- read.csv("lymphedema_dataset.csv")
set.seed(123456)

# Split the data into training and testing sets (e.g., 70% training and 30% testing)
split_index <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[split_index, ]
test_data <- df[-split_index, ]

### Logistic Regression ###
# Fit the logistic regression model on the training data
lr.model <- glm(lymphedema ~ no.of.nodes.removed + age + sex + radiation.fraction + amount.of.radiation + breast.reconstruction + chemo + axi.radioteraphy, family = "binomial", data = train_data)
# Predict the binary response on the test data
lr_predictions <- predict(lr.model, newdata = test_data, type = "response")
# Convert predictions to binary (0 or 1)
lr_predictions <- ifelse(lr_predictions > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(lr_predictions == test_data$lymphedema)

# Print the accuracy
cat("Accuracy:", accuracy, "\n")

# AUC, sensitivity and specificity
source("my.prediction.stats.R")
lymp_test <- factor(test_data$lymphedema, levels = c(0, 1))
my.pred.stats(lr_predictions, lymp_test)
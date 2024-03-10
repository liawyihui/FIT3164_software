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

### Decision Tree ###
tree.model <- rpart(lymphedema ~ no.of.nodes.removed + age + sex + radiation.fraction + amount.of.radiation + breast.reconstruction + chemo + axi.radioteraphy, data = train_data)

# Predict the binary response on the test data
tree_predictions <- predict(tree.model, test_data)
#tree_predictions <- prediction(as.numeric(tree_predictions), test_data$lymphedema)
#tree.perf <- performance(tree_predictions, "tpr", "fpr")
#tree.auc <- performance(tree_predictions, "auc")
#cat("AUC for Support Vector Machines: ", as.numeric(tree.auc@y.values))
#plot(tree.perf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

roc_curve <- roc(test_data$lymphedema, tree_predictions)
optimal_threshold <- coords(roc_curve, "best", best.method = "youden")$threshold

# Print the optimal threshold
cat("Optimal Threshold:", optimal_threshold, "\n")

tree_predictions <- ifelse(tree_predictions > optimal_threshold, 1, 0)

# Calculate accuracy
tree_accuracy <- mean(tree_predictions == test_data$lymphedema)

# Print the accuracy
cat("Decision Tree Accuracy:", tree_accuracy, "\n")

# AUC, sensitivity and specificity
source("my.prediction.stats.R")
lymp_test <- factor(test_data$lymphedema, levels = c(0, 1))
my.pred.stats(tree_predictions, lymp_test)

### ANN ###
ann.model <- neuralnet(lymphedema ~ no.of.nodes.removed + age + sex + radiation.fraction + amount.of.radiation + breast.reconstruction + chemo + axi.radioteraphy, data = train_data, , hidden = 3, linear.output = FALSE)

ann_predictions <- predict(ann.model, newdata = test_data)

roc_curve <- roc(test_data$lymphedema, ann_predictions)
optimal_threshold <- coords(roc_curve, "best", best.method = "youden")$threshold

# Print the optimal threshold
cat("Optimal Threshold:", optimal_threshold, "\n")

ann_predictions_binary <- ifelse(ann_predictions > optimal_threshold, 1, 0)

# AUC, sensitivity and specificity
source("my.prediction.stats.R")
lymp_test <- factor(test_data$lymphedema, levels = c(0, 1))
my.pred.stats(ann_predictions_binary, lymp_test)

### SVM ### # got issue
svm.train <- svm(lymphedema ~ no.of.nodes.removed + age + sex + radiation.fraction + amount.of.radiation + breast.reconstruction + chemo + axi.radioteraphy, data = train_data, type = 'C-classification', kernel = 'radial')

# Predict the binary response on the test data
svm_predictions <- predict(svm.train, newdata = test_data)

# Calculate accuracy for the SVM model
svm_accuracy <- mean(svm_predictions == test_data$lymphedema)

# Print the SVM accuracy
cat("SVM Accuracy:", svm_accuracy, "\n")

# AUC, sensitivity and specificity
source("my.prediction.stats.R")
lymp_test <- factor(test_data$lymphedema, levels = c(0, 1))
my.pred.stats(svm_predictions, lymp_test)
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

### ANN ###
set.seed(123456)
split_index <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[split_index, ]
test_data <- df[-split_index, ]

ann.model <- neuralnet(lymphedema ~ no.of.nodes.removed + age + sex + radiation.fraction + amount.of.radiation + breast.reconstruction + chemo + axi.radioteraphy, data = train_data, hidden = 3, linear.output=TRUE, stepmax=1e7)

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

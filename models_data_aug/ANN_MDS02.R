setwd("D:/FIT3164_software/models_data_aug")
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
library(smotefamily)

# reading the csv file required and creating individual data by setting a seed (my Student ID)
df <- read.csv("Lymph_dataset_raw.csv")

Table1 <- df %>%
  select(-c("id", "opd", "nam.y", "lnn","int", "le"))
#select(-c("id", "opd", "nam.y", "tax", "lnn","axi","int", "che", "fx", "Gy", "recon", "le"))

Table1$Endpoint <- factor(df$le)

# Exclude the Endpoint variable before normalizing
independent_variables <- setdiff(names(Table1), "Endpoint")

# Normalize independent variables
Table1[, independent_variables] <- scale(Table1[, independent_variables])

randomseed <- 1165# 365# 1675# 

set.seed(randomseed)

### ANN ###
split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

# Resampling training dataset
train_smote <- SMOTE(train_data[, -which(colnames(Table1) == "Endpoint")], train_data$Endpoint, K=5)
train_data <- train_smote$data
train_data$class <- factor(train_data$class)
names(train_data)[names(train_data) == "class"] <- "Endpoint"

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
registerDoMC(cores=6)
ann.model <- train(Endpoint~., data=train_data, method="pcaNNet", trControl=control, tuneLength=5)

ann_predictions <- predict(ann.model, newdata = test_data, type="prob")

ann_predictions_binary <- ifelse(ann_predictions[,1] > 0.5, 0, 1)

# Calculate accuracy
ann_accuracy <- mean(ann_predictions_binary == test_data$Endpoint)

# Print the accuracy
cat("ANN Accuracy:", ann_accuracy, "\n")

# AUC, sensitivity and specificity
#source("my.prediction.stats.R")
#lymp_test <- factor(test_data$lymphedema, levels = c(0, 1))
#my.pred.stats(tree_predictions_binary, lymp_test)

confusionMatrix(ann_predictions_binary, test_data$Endpoint, positive = "1")

ROCit_obj_test <- rocit(score=ann_predictions_binary, class=test_data$Endpoint)
ROCit_obj_test$AUC

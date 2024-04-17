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

tree.model <- rpart(Endpoint~., data = train_data)

# Predict the binary response on the test data
tree_predictions <- predict(tree.model, test_data, type="class")

# Calculate accuracy
tree_accuracy <- mean(tree_predictions == test_data$Endpoint)

# Print the accuracy
cat("Decision Tree Accuracy:", tree_accuracy, "\n")

# AUC, sensitivity and specificity
#source("my.prediction.stats.R")
#lymp_test <- factor(test_data$lymphedema, levels = c(0, 1))
#my.pred.stats(tree_predictions_binary, lymp_test)

confusionMatrix(tree_predictions, test_data$Endpoint, positive = "1")

tree_predictions_prob <- predict(tree.model, newdata = test_data)[,2]
ROCit_obj_test <- rocit(score=tree_predictions_prob, class=test_data$Endpoint)
ROCit_obj_test$AUC

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

split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

control <- trainControl(method="repeatedcv", number=10, repeats=3)
rf.model <- train(Endpoint~., data=train_data, method="rf", trControl=control, tuneLength=5)

rf_predictions <- predict(rf.model , test_data, type="prob")
rf_predictions_binary <- ifelse(rf_predictions[,1] >= 0.5, 0, 1)
accuracy <- mean(rf_predictions_binary == test_data$Endpoint)
cat("Random Forest Accuracy:", accuracy, "\n")

performance <- confusionMatrix(factor(rf_predictions_binary), test_data$Endpoint, positive = "1")
performance
performance$byClass["F1"]

ROCit_obj_test <- rocit(score=rf_predictions[,2], class=test_data$Endpoint)
ROCit_obj_test$AUC

train_predictions <- predict(rf.model, newdata = train_data, type="prob")
train_predictions_binary <- ifelse(train_predictions[,1] > 0.5, 0, 1)
accuracy <- mean(train_predictions_binary == train_data$Endpoint)
cat("Random Forest Accuracy:", accuracy, "\n")

performance <- confusionMatrix(factor(train_predictions_binary), train_data$Endpoint, positive = "1")
performance
performance$byClass["F1"]

ROCit_obj_test <- rocit(score=train_predictions[,2], class=train_data$Endpoint)
ROCit_obj_test$AUC

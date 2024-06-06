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
Table1$Endpoint <- make.names(Table1$Endpoint)

split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE)
models <- caretList(Endpoint~., data=train_data, trControl=control, methodList=c("glm", "C5.0", 'pcaNNet' ))

results <- resamples(models)
summary(results)

stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=control)

stack_predictions_prob <- predict(stack.rf, newdata = test_data, type="prob")
stack_predictions_binary <- ifelse(stack_predictions_prob > 0.5, "X0", "X1")
stack_accuracy <- mean(stack_predictions_binary == test_data$Endpoint)
cat("Stack Ensemble Accuracy:", stack_accuracy, "\n")

performance <- confusionMatrix(factor(stack_predictions_binary), factor(test_data$Endpoint), positive = "X1")
performance
performance$byClass["F1"]

stack_predictions_prob_modified <- 1 - stack_predictions_prob
ROCit_obj_test <- rocit(score=stack_predictions_prob_modified, class=test_data$Endpoint)
ROCit_obj_test$AUC

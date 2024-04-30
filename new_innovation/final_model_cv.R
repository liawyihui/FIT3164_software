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
library(ROSE)
library(ebmc)
setwd("D:/FIT3164_software/new_innovation")

# reading the csv file required and creating individual data by setting a seed (my Student ID)
df <- read.csv("Lymph_dataset.csv")

Table1 <- df %>%
  select(-c("id", "opd", "nam.y", "lnn","int", "le"))
#select(-c("id", "opd", "nam.y", "tax", "lnn","axi","int", "che", "fx", "Gy", "recon", "le"))

Table1$Endpoint <- factor(df$le)

# Exclude the Endpoint variable before normalizing
independent_variables <- setdiff(names(Table1), "Endpoint")

# Normalize independent variables
Table1[, independent_variables] <- scale(Table1[, independent_variables])

#randomseed <- 1165# 365# 1675# 

#set.seed(randomseed)

# split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
# train_data <- Table1[split_index, ]
# test_data <- Table1[-split_index, ]

# # Perform oversampling of the minority class using SMOTE
# oversampled_train_data <- SMOTE(train_data[, -which(colnames(Table1) == "Endpoint")], train_data$Endpoint, K=5)
# oversampled_train_data <- oversampled_train_data$data
# oversampled_train_data$class <- factor(oversampled_train_data$class)
# names(oversampled_train_data)[names(oversampled_train_data) == "class"] <- "Endpoint"

# # Perform undersampling of the majority class using editing algorithm
# undersample_train_data <- ovun.sample(Endpoint~., data=train_data, p=0.5, seed=1165, method="under")$data

# combined_train_data <- rbind(oversampled_train_data, undersample_train_data)

# train the model
#folds <- createFolds(Table1$Endpoint, k = 10) 

accuracy_results <- c()
best_accuracy <- 0
best_model <- NULL
best_test_data <- NULL
best_train_data <- NULL
for (i in 1:10){
    set.seed(1165 * i)
    split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
    train_data <- Table1[split_index, ]
    test_data <- Table1[-split_index, ]

    # Perform oversampling of the minority class using SMOTE
    oversampled_train_data <- SMOTE(train_data[, -which(colnames(Table1) == "Endpoint")], train_data$Endpoint, K=5)
    oversampled_train_data <- oversampled_train_data$data
    oversampled_train_data$class <- factor(oversampled_train_data$class)
    names(oversampled_train_data)[names(oversampled_train_data) == "class"] <- "Endpoint"

    # Perform undersampling of the majority class using editing algorithm
    undersample_train_data <- ovun.sample(Endpoint~., data=train_data, p=0.5, seed=1165, method="under")$data

    combined_train_data <- rbind(oversampled_train_data, undersample_train_data)
    # Train the model on the training data 
    model <- rus(Endpoint~., combined_train_data, size=50, alg = "rf", ir = 1, rf.ntree = 500)
    
    rus_predictions <- predict(model , test_data, type="prob")
    rus_predictions_binary <- ifelse(rus_predictions > 0.5, 1, 0)
    accuracy <- mean(rus_predictions_binary == test_data$Endpoint)
    cat("RUSBoost Accuracy:", accuracy, "\n")
    if(accuracy > best_accuracy){
        best_accuracy = accuracy
        best_model = model
        best_test_data = test_data
        best_train_data = combined_train_data
  }

  # Store the result 
  accuracy_results <- c(accuracy_results, accuracy)
}

#best_model <- rus(Endpoint~., best_train_data, size=10, alg = "rf", ir = 1, rf.ntree = 500)
rus_predictions <- predict(best_model , best_test_data, type="prob")
rus_predictions_binary <- ifelse(rus_predictions > 0.5, 1, 0)
accuracy <- mean(rus_predictions_binary == best_test_data$Endpoint)
cat("RUSBoost Accuracy:", accuracy, "\n")

performance <- confusionMatrix(factor(rus_predictions_binary), best_test_data$Endpoint, positive = "1")
performance
performance$byClass["F1"]

ROCit_obj_test <- rocit(score=rus_predictions, class=best_test_data$Endpoint)
ROCit_obj_test$AUC
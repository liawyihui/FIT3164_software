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
library(h2o)

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

randomseed <- 1165# 365# 1675# 

set.seed(randomseed)

split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

minority_data <- train_data[train_data$Endpoint == "1", ]
majority_data <- train_data[train_data$Endpoint == "0", ]
majority_size <- nrow(majority_data)
minority_size <- nrow(minority_data)

balanced_datasets <- list()
start_pos <- 1
while(TRUE) {
    if ((majority_size - start_pos) >= minority_size) {
        subset <- majority_data[start_pos:(start_pos + minority_size),]
        subset <- rbind(subset, minority_data)
        balanced_datasets <- append(balanced_datasets, list(subset))
        start_pos <- start_pos + minority_size + 1
    } else {
        diff <- (minority_size - (majority_size - start_pos))
        subset <- majority_data[(start_pos - diff):majority_size,]
        subset <- rbind(subset, minority_data)
        balanced_datasets <- append(balanced_datasets, list(subset))
        break
    }
}

models <- list()
for (data in balanced_datasets){
    #control <- trainControl(method="repeatedcv", number=10, repeats=3)
    #ada.model <- train(Endpoint~., data=train_data, method="AdaBoost.M1", trControl=control, tuneLength=5)
    data <- as.h2o(data)
    ada.model <- h2o.randomForest(
                          y = "Endpoint",
                          training_frame = data,
                          ntrees = 200,
                          nfolds = 10,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)
    test_h2o <- as.h2o(test_data)
    predictions <- h2o.predict(ada.model, newdata = test_h2o)
    ada_predictions_binary <- as.vector(ifelse(predictions[,3] >= 0.5, 0, 1))
    accuracy <- mean(test_data$Endpoint == ada_predictions_binary)
    print(accuracy)
    performance <- confusionMatrix(factor(ada_predictions_binary), test_data$Endpoint, positive = "1")
    print(performance)
    models <- append(models, list(ada.model))
}

model <- h2o.stackedEnsemble(y="Endpoint", training_frame = as.h2o(balanced_datasets[[31]]), model_id = NULL, validation_frame = NULL, base_models = models)
test_h2o <- as.h2o(test_data)
predictions <- h2o.predict(model, newdata = test_h2o)
ada_predictions_binary <- as.vector(ifelse(predictions[,3] >= 0.5, 0, 1))
accuracy <- mean(test_data$Endpoint == ada_predictions_binary)

performance <- confusionMatrix(factor(ada_predictions_binary), test_data$Endpoint, positive = "1")
print(performance)
print(performance$byClass["F1"])

ROCit_obj_test <- rocit(score=as.vector(predictions[,2]), class=test_data$Endpoint)
print(ROCit_obj_test$AUC)

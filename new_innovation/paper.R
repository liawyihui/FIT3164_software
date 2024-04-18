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

### RF ###
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
        subset <- majority_data[start_pos:majority_size,]
        subset <- rbind(subset, minority_data)
        balanced_datasets <- append(balanced_datasets, list(subset))
        break
    }
}

for (data in balanced_datasets){
    #control <- trainControl(method="repeatedcv", number=10, repeats=3)
    #ada.model <- train(Endpoint~., data=train_data, method="AdaBoost.M1", trControl=control, tuneLength=5)
    ada.model <- boosting(Endpoint~., data = data, boos = TRUE, mfinal = 100)
    ada_predictions <- predict(ada.model , test_data)$prob
    ada_predictions_binary <- ifelse(ada_predictions[,1] >= 0.5, 0, 1)
    accuracy <- mean(ada_predictions_binary == test_data$Endpoint)
    cat("AdaBoost Accuracy:", accuracy, "\n")

    performance <- confusionMatrix(factor(ada_predictions_binary), test_data$Endpoint, positive = "1")
    print(performance)
    print(performance$byClass["F1"])

    ROCit_obj_test <- rocit(score=ada_predictions[,2], class=test_data$Endpoint)
    print(ROCit_obj_test$AUC)
}
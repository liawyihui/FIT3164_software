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
library(caretEnsemble)
library(ebmc)

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

# train_smote <- SMOTE(train_data[, -which(colnames(Table1) == "Endpoint")], train_data$Endpoint, K=10)
# train_smote_minority <- train_smote$data
# train_smote_minority$class <- factor(train_smote_minority$class)
# names(train_smote_minority)[names(train_smote_minority) == "class"] <- "Endpoint"
# train_smote_minority <- train_smote_minority[train_smote_minority$Endpoint == "1", ]

splitted_datasets <- list()
majority_subset1 <- majority_data[1:floor(majority_size/2),]
majority_subset1 <- as.data.frame(rbind(majority_subset1, minority_data))
majority_subset1$Endpoint <- as.numeric(majority_subset1$Endpoint) - 1
subset1_smote <- SMOTE(majority_subset1[, -which(colnames(majority_subset1) == "Endpoint")], majority_subset1$Endpoint, K=10)
majority_subset1_smote <- subset1_smote$data
majority_subset1_smote$class <- factor(majority_subset1_smote$class)
names(majority_subset1_smote)[names(majority_subset1_smote) == "class"] <- "Endpoint"
splitted_datasets <- append(splitted_datasets, list(majority_subset1_smote))

majority_subset2 <- majority_data[(floor(majority_size/2) + 1):majority_size,]
majority_subset2 <- as.data.frame(rbind(majority_subset2, minority_data))
majority_subset2$Endpoint <- as.numeric(majority_subset2$Endpoint) - 1
subset2_smote <- SMOTE(majority_subset2[, -which(colnames(majority_subset2) == "Endpoint")], majority_subset2$Endpoint, K=10)
majority_subset2_smote <- subset2_smote$data
majority_subset2_smote$class <- factor(majority_subset2_smote$class)
names(majority_subset2_smote)[names(majority_subset2_smote) == "class"] <- "Endpoint"
splitted_datasets <- append(splitted_datasets, list(majority_subset2_smote))

# balanced_datasets <- list()
# start_pos <- 1
# while(TRUE) {
#     if ((majority_size - start_pos) >= minority_size) {
#         subset <- majority_data[start_pos:(start_pos + minority_size),]
#         subset <- rbind(subset, minority_data)
#         balanced_datasets <- append(balanced_datasets, list(subset))
#         start_pos <- start_pos + minority_size + 1
#     } else {
#         diff <- (minority_size - (majority_size - start_pos))
#         subset <- majority_data[(start_pos - diff):majority_size,]
#         subset <- rbind(subset, minority_data)
#         balanced_datasets <- append(balanced_datasets, list(subset))
#         break
#     }
# }

models <- list()
for (data in splitted_datasets){
    # model <- rus(Endpoint~., data, size=20, alg = "rf", ir = 5, rf.ntree = 50, svm.ker = "radial")

    # rus_predictions <- predict(model , test_data, type="prob")
    # rus_predictions_binary <- ifelse(rus_predictions > 0.5, 1, 0)
    # accuracy <- mean(rus_predictions_binary == test_data$Endpoint)
    # cat("RUSBoost Accuracy:", accuracy, "\n")
    
    # performance <- confusionMatrix(factor(rus_predictions_binary), test_data$Endpoint, positive = "1")
    # print(performance)
    # print(performance$byClass["F1"])

    # ROCit_obj_test <- rocit(score=rus_predictions, class=test_data$Endpoint)
    # print(ROCit_obj_test$AUC)

    control <- trainControl(method="repeatedcv", number=10, repeats=3)
    rf.model <- train(Endpoint~., data=data, method="rf", trControl=control, tuneLength=5)

    rf_predictions <- predict(rf.model , test_data, type="prob")
    rf_predictions_binary <- ifelse(rf_predictions[,1] >= 0.5, 0, 1)
    accuracy <- mean(rf_predictions_binary == test_data$Endpoint)
    cat("Random Forest Accuracy:", accuracy, "\n")

    performance <- confusionMatrix(factor(rf_predictions_binary), test_data$Endpoint, positive = "1")
    print(performance)
    print(performance$byClass["F1"])

    ROCit_obj_test <- rocit(score=rf_predictions[,2], class=test_data$Endpoint)
    print(ROCit_obj_test$AUC)
}

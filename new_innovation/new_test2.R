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
library(FNN)  # For k-nearest neighbors calculation

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

split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

# Step 1: Calculate local density
k <- 5  # Number of neighbors to consider
distances <- knn.dist(train_data[, independent_variables], k=k)
local_density <- 1 / (apply(distances, 1, mean) + 1e-6)  # Avoid division by zero

# Step 2: Determine undersampling ratio
calculate_undersampling_ratio <- function(local_density) {
  # Define a mapping function, you can experiment with different mappings
  # For example, you can use a linear or exponential function
  # Here, let's use a linear function
  undersampling_ratio <- 1 - (local_density - min(local_density)) / (max(local_density) - min(local_density))
  return(undersampling_ratio)
}

undersampling_ratio <- calculate_undersampling_ratio(local_density)

# Step 3: Apply undersampling
undersampled_indices <- which(runif(length(undersampling_ratio)) < undersampling_ratio)

train_data_undersampled <- train_data[undersampled_indices, ]

ada.model <- train(Endpoint~., data=train_data_undersampled, method="rf")
#ada.model <- boosting(Endpoint~., data = train_data_undersampled, boos = TRUE, mfinal = 100)
ada_predictions <- predict(ada.model , test_data, type="prob")
ada_predictions_binary <- ifelse(ada_predictions[,1] >= 0.5, 0, 1)
accuracy <- mean(ada_predictions_binary == test_data$Endpoint)
cat("AdaBoost Accuracy:", accuracy, "\n")

performance <- confusionMatrix(factor(ada_predictions_binary), test_data$Endpoint, positive = "1")
print(performance)
print(performance$byClass["F1"])

ROCit_obj_test <- rocit(score=ada_predictions[,2], class=test_data$Endpoint)
print(ROCit_obj_test$AUC)

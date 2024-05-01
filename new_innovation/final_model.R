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

# Perform oversampling of the minority class using SMOTE
oversampled_train_data <- SMOTE(train_data[, -which(colnames(Table1) == "Endpoint")], train_data$Endpoint, K=5)
oversampled_train_data <- oversampled_train_data$data
oversampled_train_data$class <- factor(oversampled_train_data$class)
names(oversampled_train_data)[names(oversampled_train_data) == "class"] <- "Endpoint"

# Perform undersampling of the majority class using editing algorithm
undersample_train_data <- ovun.sample(Endpoint~., data=train_data, p=0.5, seed=1165, method="under")$data

combined_train_data <- rbind(oversampled_train_data, undersample_train_data)

model <- rus(Endpoint~., combined_train_data, size=30, alg = "rf", ir = 1, rf.ntree = 100)

rus_predictions <- predict(model, test_data, type="prob")
rus_predictions_binary <- ifelse(rus_predictions > 0.5, 1, 0)
accuracy <- mean(rus_predictions_binary == test_data$Endpoint)
cat("RUSBoost Accuracy:", accuracy, "\n")

performance <- confusionMatrix(factor(rus_predictions_binary), test_data$Endpoint, positive = "1")
performance
performance$byClass["F1"]

ROCit_obj_test <- rocit(score=rus_predictions, class=test_data$Endpoint)
ROCit_obj_test$AUC

importance_list <- list()
for (i in 1:length(model$weakLearners)) {
    # Calculate variable importance for the ith weak learner
    importance <- varImp(model$weakLearners[[i]], scale = FALSE)
    
    # Convert importance to a data frame
    var_imp_df <- data.frame(variable = rownames(importance), score = importance[,1], stringsAsFactors = FALSE)
    
    # Convert score to numeric
    var_imp_df$score <- as.numeric(var_imp_df$score)

    # Store variable importance scores in the list
    importance_list[[i]] <- var_imp_df
}

mean_scores <- list()
for (variable in unique(importance_list[[1]]$variable)) {
    # Extract scores for the current variable from all weak learners
    scores <- sapply(importance_list, function(df) {
        df[df$variable == variable, "score"]
    })
    
    # Calculate the mean score for the current variable
    mean_score <- mean(scores, na.rm = TRUE)
    
    # Store the mean score
    mean_scores[[variable]] <- mean_score
}

# Create a data frame with variable names and mean importance scores
mean_var_imp_df <- data.frame(variable = names(mean_scores), score = unlist(mean_scores))

# Sort by variable name
mean_var_imp_df <- mean_var_imp_df[order(mean_var_imp_df$score, decreasing = TRUE),]

# Output the mean variable importance data frame
print(mean_var_imp_df)

write.xlsx(mean_var_imp_df, file = "variance_importance.xlsx")

# importance <- varImp(model$weakLearners[[1]], scale=FALSE)
# var_imp_df <- data.frame(cbind(variable = rownames(importance), score = importance[,1]))
# var_imp_df$score <- as.double(var_imp_df$score)
# var_imp_df[order(var_imp_df$score,decreasing = TRUE),]

#write.xlsx(data.frame(FPR = ROCit_obj_test$FPR, TPR = ROCit_obj_test$TPR), file = "ROC_test.xlsx")

# train_sizes <- seq(0.1, 0.9, by = 0.1)
# train_errors <- rep(NA, length(train_sizes))
# test_errors <- rep(NA, length(train_sizes))

# for (i in seq_along(train_sizes)) {
#   split_index <- createDataPartition(y = Table1$Endpoint, p = train_sizes[i], list = FALSE)
#   train_data <- Table1[split_index, ]
#   test_data <- Table1[-split_index, ]
  
#   # Perform oversampling of the minority class using SMOTE
#   oversampled_train_data <- SMOTE(train_data[, -which(colnames(Table1) == "Endpoint")], train_data$Endpoint, K=5)
#   oversampled_train_data <- oversampled_train_data$data
#   oversampled_train_data$class <- factor(oversampled_train_data$class)
#   names(oversampled_train_data)[names(oversampled_train_data) == "class"] <- "Endpoint"

#   # Perform undersampling of the majority class using editing algorithm
#   undersample_train_data <- ovun.sample(Endpoint~., data=train_data, p=0.5, seed=1165, method="under")$data

#   combined_train_data <- rbind(oversampled_train_data, undersample_train_data)

#   # Train model on partial data
#   partial_model <- rus(Endpoint~., combined_train_data, size=30, alg = "rf", ir = 1, rf.ntree = 100)
  
#   # Predict on training and test data
#   partial_train_predictions <- predict(partial_model, train_data, type="prob")
#   partial_train_predictions_binary <- ifelse(partial_train_predictions > 0.5, 1, 0)
#   partial_test_predictions <- predict(partial_model, test_data, type="prob")
#   partial_test_predictions_binary <- ifelse(partial_test_predictions > 0.5, 1, 0)
  
#   # Compute errors
#   train_errors[i] <- 1 - mean(partial_train_predictions_binary == train_data$Endpoint)
#   test_errors[i] <- 1 - mean(partial_test_predictions_binary == test_data$Endpoint)
# }

# plot(train_sizes, train_errors, type = "b", col = "blue", ylim = c(0, max(train_errors, test_errors)), xlab = "Training Size", ylab = "Error", main = "Learning Curves")
# points(train_sizes, test_errors, type = "b", col = "red")
# legend("topright", legend = c("Training Error", "Test Error"), col = c("blue", "red"), lty = 1, cex = 0.8)
# Loading the packages needed
library(dplyr)
library(ggplot2)
library(lattice)
library(caret) # For confusion matrix
library(openxlsx) # For writing data to excel file
library(ROCit) # For finding AUC
library(smotefamily) # For oversampling
library(ROSE) # For oversampling
library(ebmc) # For RUSBoost

# Read the csv file required
df <- read.csv("Lymph_dataset_train_model.csv")

# Filter our the y variable
Table1 <- df %>% select(-c("le"))

# Factor the y variable and assign to the new column, "Endpoint" in Table1
Table1$Endpoint <- factor(df$le)

# Exclude the Endpoint variable before normalizing
independent_variables <- setdiff(names(Table1), "Endpoint")

# Normalize independent variables
Table1[, independent_variables] <- scale(Table1[, independent_variables])

# Setting seed
randomseed <- 1165

set.seed(randomseed)

# Split dataset into 80% training data and 20% testing data
split_index <- createDataPartition(y = Table1$Endpoint, p = 0.8, list = FALSE)
train_data <- Table1[split_index, ]
test_data <- Table1[-split_index, ]

# Perform oversampling of the minority class using SMOTE
oversampled_train_data <- SMOTE(train_data[, -which(colnames(Table1) == "Endpoint")], train_data$Endpoint, K = 5)
oversampled_train_data <- oversampled_train_data$data
oversampled_train_data$class <- factor(oversampled_train_data$class)
names(oversampled_train_data)[names(oversampled_train_data) == "class"] <- "Endpoint"

# Perform undersampling of the majority class
undersample_train_data <- ovun.sample(Endpoint ~ ., data = train_data, p = 0.5, seed = 1165, method = "under")$data

# Combine both oversampled and undersampled data
combined_train_data <- rbind(oversampled_train_data, undersample_train_data)

# Train the model using RUSBoost
model <- rus(Endpoint ~ ., combined_train_data, size = 30, alg = "rf", ir = 1, rf.ntree = 100)

# Make prediction on the testing dataset and finding the accuracy
rus_predictions <- predict(model, test_data, type = "prob")
rus_predictions_binary <- ifelse(rus_predictions > 0.5, 1, 0)
accuracy <- mean(rus_predictions_binary == test_data$Endpoint)
cat("RUSBoost Accuracy:", accuracy, "\n")

# Find results for other performance metrics
performance <- confusionMatrix(factor(rus_predictions_binary), test_data$Endpoint, positive = "1")
performance
performance$byClass["F1"]

# Write confusion matrix to excel file
cm <- as.data.frame(performance$table)
write.xlsx(cm, file = "confusion_matrix.xlsx")

# Find the AUC
ROCit_obj_test <- rocit(score = rus_predictions, class = test_data$Endpoint)
ROC_data_test <- data.frame(
    FPR = ROCit_obj_test$FPR,
    TPR = ROCit_obj_test$TPR
)
ROCit_obj_test$AUC

# Write the ROC data to excel file
write.xlsx(ROC_data_test, file = "ROC_test.xlsx")

# Find relative variable importance for each weak learners
importance_list <- list()
for (i in 1:length(model$weakLearners)) {
    # Calculate variable importance for the ith weak learner
    importance <- varImp(model$weakLearners[[i]], scale = FALSE)

    # Convert importance to a data frame
    var_imp_df <- data.frame(variable = rownames(importance), score = importance[, 1], stringsAsFactors = FALSE)

    # Convert score to numeric
    var_imp_df$score <- as.numeric(var_imp_df$score)

    # Store variable importance scores in the list
    importance_list[[i]] <- var_imp_df
}

# Find the average the respective variable for the relative variance importance scores generated for each weak learner
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
mean_var_imp_df <- mean_var_imp_df[order(mean_var_imp_df$score, decreasing = TRUE), ]

# Output the mean variable importance data frame
print(mean_var_imp_df)

# Write results to excel file
write.xlsx(mean_var_imp_df, file = "variance_importance.xlsx")

# Save model to RData file
save(model, file = "final_model.RData")

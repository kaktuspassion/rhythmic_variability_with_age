# Assignment 2. Rhythmic variability between speakers
# Computational Processing of Speech Rhythm, University of Zurich
# 8 Dec. 2023
# Rong Li, 22-738-009, Olat:ronli
# Yating Pan, 22-733-380, Olat:yating
# Haonan Chen, 22-738-512, Olat:haonch

rm(list=ls())

# Libraries
library(lattice)
library(dplyr)
library(dlookr)
library(tidyr)
library(ggplot2)
library(caret)        # pre-processing
library(rpart)        # decision tree
library(partykit)     # conditional inference tree
library(randomForest) # random forest classifier
library(pROC)         # ROC and AUC


# Read the CSV files
CV_file <- read.csv("./CV_measures.csv", sep = ";")
Intensity_file <- read.csv("./intensityVariability.csv", sep = "\t")

# Merge based on speakerID and sentenceID
merged_data <- merge(Intensity_file, CV_file, by.x = c("speakerID", "sentenceID"), by.y = c("speaker", "sentence"))
# summary(merged_data)
describe(merged_data)

# Select relevant metrics for further analysis
response <- 'age'
select_metrics <- c('percentV_tier3', 'deltaV_tier3', 'deltaC_tier3', 'rPVI_C_tier3', 'stdevP', 'varcoP', 'rPVIp', 'nPVIp')
dd <- merged_data %>% select(c(response, select_metrics))

set.seed(24) # seed
dd <- dd %>% drop_na(age)

# Data Partitioning
index <- createDataPartition(dd$age, p = 0.8, list = FALSE) 
train <- dd[index, ]
test <- dd[-index, ]
# Check the distribution of classes
dd %>%
  group_by(age) %>%
  summarise(n = n(), prop = n() / nrow(dd))
# output: o = 599, y = 960

# Class balancing: oversampling 
train$age <- as.factor(train$age)
train_upsamp <- upSample(
  x=train[, 2:9], y=train$age, list=FALSE, yname="age"
)
# Move 'age' to the front of the dataframe
train_upsamp <- train_upsamp %>% select(age, everything())

train_upsamp %>%
  group_by(age) %>%
  summarise(n = n(), prop = n() / nrow(train_upsamp))


######## 1. Decision tree

dec.tree <- rpart(
  formula = age ~ ., data = train_upsamp, method = "class"
)
printcp(dec.tree) # display the results
summary(dec.tree) # summary of splits

# Visualization of Decision tree
plot(dec.tree, uniform = TRUE, main = "Classification Tree for Age") 
text(dec.tree, use.n = TRUE, all=TRUE, cex=.8)

# Feature Importance
feature_importance <- varImp(dec.tree)
# Arrange by Overall column in descending order
feature_importance <- arrange(feature_importance, desc(Overall))
print(feature_importance)


######## 2. Conditional Inference Tree
ct <- ctree(formula = age ~ ., data = train_upsamp)
plot(ct, main="Conditional Inference Tree for Type")

# Print metrics' importance
var_importance <- varimp(ct)
sorted_var_importance <- sort(var_importance, decreasing = TRUE) # sort values in descending order
print(sorted_var_importance)
# Plot variable importance
plot(sorted_var_importance, main = "Feature Importance")


######## 3. “Simpler” trees by limiting the depth of the tree

# Experiment by setting maximum depth to avoid overfitting
simple.ct <- ctree(
  formula = age ~ ., data = train_upsamp, control = ctree_control(maxdepth = 2)
)
plot(simple.ct)


######## 4. Random Forest classifier

# Use cross-validation to find the best maxnodes and ntree hyperparameters for the Random Forest classifier
# Create a grid of hyperparameters to search
hyperparam_grid <- expand.grid(
  maxnodes = seq(10, 100, by = 10), 
  ntree = seq(20, 200, by = 20)
)
test.features <- test[, 2:9]
test$age <- as.factor(test$age)

# Initialize a container to store results
results <- data.frame(maxnodes = integer(0), ntree = integer(0), accuracy = numeric(0))

# Perform cross-validation to find the best hyperparameters
for (i in 1:nrow(hyperparam_grid)) {
  # Extract hyperparameters
  maxnodes_val <- hyperparam_grid$maxnodes[i]
  ntree_val <- hyperparam_grid$ntree[i]
  
  # Create and train the Random Forest model
  rf <- randomForest(
    x = train_upsamp[, 2:9],
    y = train_upsamp$age,
    maxnodes = maxnodes_val,
    ntree = ntree_val,
    importance = TRUE
  )
  
  # Predict on the test set
  pred_rf <- predict(rf, newdata = test.features, type = "class")
  
  # Calculate accuracy
  acc_rf <- confusionMatrix(pred_rf, test$age)$overall
  
  # Store results
  results <- rbind(results, data.frame(maxnodes = maxnodes_val, ntree = ntree_val, accuracy = acc_rf[[1]]))
}

# Find the best hyperparameters based on accuracy
best_hyperparams <- results[which.max(results$accuracy), ]
best_hyperparams

# Train the final Random Forest model with the best hyperparameters
rf <- randomForest(
  x = train_upsamp[, 2:9],
  y = train_upsamp$age,
  maxnodes = best_hyperparams$maxnodes,
  ntree = best_hyperparams$ntree,
  importance = TRUE
)
rf

# Variable importance 
varImpPlot(rf, main="Feature Importance")
print(importance(rf))


######## Comparison of all the 3 models on the test set
test.features <- test[, 2:9]
test$age <- as.factor(test$age)

pred.dec.tree <- predict(dec.tree, newdata = test.features, type="class") 
pred.ct <- predict(ct, newdata = test.features, type="response") 
pred.ct.simple <- predict(simple.ct, newdata = test.features, type="response")
pred.rf <- predict(rf, newdata = test.features, type="class")

acc.dec.tree <- confusionMatrix(pred.dec.tree, test$age)$overall 
acc.ct <- confusionMatrix(pred.ct, test$age)$overall
acc.simple.ct <- confusionMatrix(pred.ct.simple, test$age)$overall
acc.rf <- confusionMatrix(pred.rf, test$age)$overall

compare_df <- data.frame(
  model = c("rpart", "ctree", "simple ctree", "randomForest"),
  accuracy = rbind(acc.dec.tree[[1]], acc.ct[[1]], acc.simple.ct[[1]], acc.rf[[1]])
)
arrange(compare_df, accuracy)
# Results: 
# model  accuracy
# 1        rpart 0.6752412
# 2        ctree 0.7009646
# 3 simple ctree 0.7009646
# 4 randomForest 0.7331190


######## ROC plot

# ROC for Decision Tree
pred.probs_dt <- predict(dec.tree, na.roughfix(test[,2:9]), type="prob")
# Remove extra spaces from column names
colnames(pred.probs_dt) <- trimws(colnames(pred.probs_dt))
# "o" is the class of interest (old speakers)
pold_dt <- pred.probs_dt[, "o"]
# ROC for Decision Tree
roc_dt <- multiclass.roc(test$age, pold_dt, percent=TRUE)
print(roc_dt)
# focusing only on old speakers
r_old_dt <- roc_dt[['rocs']][[1]] 
plot.roc(
  r_old_dt, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2), grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="lightblue", print.thres=TRUE,
  main= 'ROC Curve for Decision Tree'
)
# AUC: 73.0%


# ROC for Conditional Inference Tree
pred.probs_ct <- predict(ct, na.roughfix(test[,2:9]), type="prob")
# Remove extra spaces from column names
colnames(pred.probs_ct) <- trimws(colnames(pred.probs_ct))
# "o" is the class of interest (old speakers)
pold_ct <- pred.probs_ct[, "o"]
# ROC for Conditional Inference Tree
roc_ct <- multiclass.roc(test$age, pold_ct, percent=TRUE)
print(roc_ct)
# focusing only on old speakers
r_old_ct <- roc_ct[['rocs']][[1]] 
plot.roc(
  r_old_ct, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2), grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="lightblue", print.thres=TRUE,
  main= 'ROC Curve for Conditional Inference Tree'
)
# AUC: 78.4%


# ROC for “Simpler” Conditional Inference Tree
pred.probs_sct <- predict(simple.ct, na.roughfix(test[,2:9]), type="prob")
# Remove extra spaces from column names
colnames(pred.probs_sct) <- trimws(colnames(pred.probs_sct))
# "o" is the class of interest (old speakers)
pold_sct <- pred.probs_sct[, "o"]
# ROC for Conditional Inference Tree
roc_sct <- multiclass.roc(test$age, pold_sct, percent=TRUE)
print(roc_sct)
# focusing only on old speakers
r_old_sct <- roc_sct[['rocs']][[1]] 
plot.roc(
  r_old_sct, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2), grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="lightblue", print.thres=TRUE,
  main= 'ROC Curve for Simplified Conditional Inference Tree'
)
# AUC: 72.6%


# ROC for Random Forest
pred.probs_rf <- predict(rf,  na.roughfix(test[,2:9]), type="prob")
# Remove extra spaces from column names
colnames(pred.probs_rf) <- trimws(colnames(pred.probs_rf))
# "o" is the class of interest (old speakers)
pold_rf <- pred.probs_rf[, "o"]
# ROC for Random Forest
roc_rf <- multiclass.roc(test$age, pold_rf, percent=TRUE)
print(roc_ct)
# focusing only on old speakers
r_old_rf <- roc_rf[['rocs']][[1]] 
plot.roc(
  r_old_rf, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2), grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="lightblue", print.thres=TRUE,
  main= 'ROC Curve for Random Forest'
)
# AUC: 82.9%


######## Random Forest Model Summary

# Generate the confusion matrix
conf_matrix <- confusionMatrix(pred.rf, test$age)
print(conf_matrix)

# Generate a model summary table
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1 <- conf_matrix$byClass["F1"]

model_summary <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1)
)

print(model_summary)


######## Random Forest Model Feature Importance

varImpPlot(rf, main="Feature Importance")
print(importance(rf))

importance_df <- as.data.frame(importance(rf))

# Subset the data frame by feature type
durational_features_df <- importance_df %>%
  filter(row.names(importance_df) %in% c("percentV_tier3", "deltaV_tier3", "deltaC_tier3", "rPVI_C_tier3"))

intensity_features_df <- importance_df %>%
  filter(row.names(importance_df) %in% c("stdevP", "varcoP", "rPVIp", "nPVIp"))

# Calculate the sums for each set and each importance metric
sum_durational_accuracy <- sum(durational_features_df["MeanDecreaseAccuracy"])
sum_intensity_accuracy <- sum(intensity_features_df["MeanDecreaseAccuracy"])
sum_durational_gini <- sum(durational_features_df["MeanDecreaseGini"])
sum_intensity_gini <- sum(intensity_features_df["MeanDecreaseGini"])

# Output the sums
sum_durational_accuracy
sum_intensity_accuracy
sum_durational_gini
sum_intensity_gini


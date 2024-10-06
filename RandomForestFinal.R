# Load necessary libraries
library(readxl)
library(dplyr)
library(randomForest)
library(caret)

# Load the data from the Excel file
file_path <- "data.xlsx"
data <- read_excel(file_path)

# Convert relevant columns to factors
data$AgeGroup <- as.factor(data$AgeGroup)
data$LocationType <- as.factor(data$LocationType)
data$ROVDivision <- as.factor(data$ROVDivision)
data$TerritorialAuthority <- as.factor(data$TerritorialAuthority)
data$PersonOrOrganization <- as.factor(data$PersonOrOrganization)
data$AnzsocDivision <- as.factor(data$AnzsocDivision)


str(data)

# Convert the date column to Date type
data$Date <- as.Date(data$Date)

# Filter the data for the past 5 years (assuming date filter is required)
recent_data <- data %>%
  filter(Date >= (Sys.Date() - 5 * 365))

# Remove missing values
data_clean <- na.omit(recent_data)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data_clean$AnzsocDivision, p = 0.7, list = FALSE)
train_data <- data_clean[trainIndex, ]
test_data <- data_clean[-trainIndex, ]

# Train a Random Forest model
rf_model <- randomForest(AnzsocDivision ~ AgeGroup + LocationType + ROVDivision + PersonOrOrganization, 
                         data = train_data, 
                         ntree = 100,  # Number of trees
                         mtry = 3,     # Number of variables randomly sampled as candidates at each split
                         importance = TRUE)

# Print the random forest model summary
print(rf_model)

# Variable importance plot
varImpPlot(rf_model)

# Predict on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model using a confusion matrix
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data$AnzsocDivision)
print(conf_matrix_rf)

# Convert confusion matrix to a matrix if not already
conf_matrix_mat <- as.matrix(conf_matrix_rf$table)

print(conf_matrix_mat)


accuracy <- conf_matrix_rf$overall['Accuracy']
print(paste("Accuracy:", accuracy))




# Calculate average precision, recall, and F1 score
avg_precision <- mean(precision)
avg_recall <- mean(recall)


# Print the results
print(paste("Average Precision:", avg_precision))
print(paste("Average Recall:", avg_recall))



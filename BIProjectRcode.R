# Load necessary libraries
library(dplyr)
library(randomForest)
library(caret)
library(knitr)

# Read dataframe
data <- read.csv("data2.csv")

data$AgeGroup <- as.factor(data$AgeGroup)
data$LocationType <- as.factor(data$LocationType)
data$ROVDivision <- as.factor(data$ROVDivision)
data$TerritorialAuthority <- as.factor(data$TerritorialAuthority)
data$PersonOrOrganization <- as.factor(data$PersonOrOrganization)
data$AnzsocDivision <- as.factor(data$AnzsocDivision)
data$PoliceArea <- as.factor(data$PoliceArea)

str(data)

data$Date <- as.Date(data$Date)

# Remove missing values
data_clean <- na.omit(data)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data_clean$AnzsocDivision, p = 0.7, list = FALSE)
train_data <- data_clean[trainIndex, ]
test_data <- data_clean[-trainIndex, ]


# Train a Random Forest model
rf_model <- randomForest(AnzsocDivision ~ AgeGroup + LocationType + ROVDivision + PersonOrOrganization + PoliceArea, 
                         data = train_data, 
                         ntree = 30,
                         mtry = 3,     
                         importance = TRUE)
# ntree = 30,  # Number of trees
# mtry = 3,  Number of variables randomly sampled as candidates at each split

# Print the random forest model summary
print(rf_model)

# Plot
plot(rf_model)

# Variable importance plot
varImpPlot(rf_model)

# Predict on the test set
rf_predictions <- predict(rf_model, newdata = test_data)


# Confusion matrix object is created 
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data$AnzsocDivision)

# Convert the confusion matrix into a data frame 
conf_matrix_df <- as.data.frame(conf_matrix_rf$table) 

# Extract the first word from the Prediction labels
conf_matrix_df$Prediction <- sapply(strsplit(as.character(conf_matrix_df$Prediction), " "), `[`, 1)

# Plot the confusion matrix 
ggplot(conf_matrix_df, aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(x = "Predicted Class", y = "Actual Class", title = "Confusion Matrix") +
  theme_minimal(base_size = 10) +  # Increase the overall base size of the plot
  geom_text(aes(label = Freq), vjust = 1, size = 3) +  # Increase size of text in cells
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  # Increase and rotate x-axis labels
        axis.text.y = element_text(size = 8),  # Reduce y-axis label size
        plot.title = element_text(size = 10, hjust = 0.5))  


# Extract Overall Statistics from confusion matrix
overall_stats <- data.frame(
  Statistic = c("Accuracy", "95% CI", "No Information Rate", "P-Value [Acc > NIR]", "Kappa"),
  Value = c(
    round(conf_matrix_rf$overall['Accuracy'], 4), 
    paste0("(", round(conf_matrix_rf$overall['AccuracyLower'], 3), ", ", round(conf_matrix_rf$overall['AccuracyUpper'], 3), ")"),
    round(conf_matrix_rf$overall['AccuracyNull'], 2),
    format.pval(conf_matrix_rf$overall['AccuracyPValue'], digits = 3, scientific = TRUE),
    round(conf_matrix_rf$overall['Kappa'], 3)
  )
)

# Display the table in a nicely formatted way
kable(overall_stats, caption = "Overall Statistics for the Random Forest Model")



# Calculate accuracy
accuracy <- conf_matrix_rf$overall['Accuracy']
print(paste("Accuracy:", accuracy))





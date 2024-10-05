library(randomForest)
library(dplyr)    # For data manipulation

# Load your dataset
data <- read.csv("data.csv")

# Check for missing values
summary(data)

# Handle missing values if needed (using na.omit here as an example)
data <- na.omit(data)



# Assuming your dataset is called 'data'
colnames(data)

# Convert categorical variables to factors
data$LocationType <- as.factor(data$LocationType)
data$AgeGroup <- as.factor(data$AgeGroup)
data$AnzsocDivision <- as.factor(data$AnzsocDivision)
data$PoliceArea <- as.factor(data$PoliceArea)
data$TerritorialAuthority <- as.factor(data$TerritorialAuthority)

# Remove unnecessary high-cardinality categorical variables
data <- data %>% select(-TerritorialAuthority, -ROVDivision, -PersonOrOrganization)

# Check for missing values and omit if necessary
data <- na.omit(data)

str(data)


set.seed(123)  # Setting seed for reproducibility
sample <- sample.int(n = nrow(data), size = floor(.7*nrow(data)), replace = FALSE)
train <- data[sample, ]
test  <- data[-sample, ]


rf_model <- randomForest(LocationType ~ Victimisations + AgeGroup + AnzsocDivision + PoliceArea,
                         data = train, 
                         importance = TRUE,
                         ntree = 500)




# Predicting on the test set
predictions <- predict(rf_model, newdata = test)

# Confusion Matrix
confusion_matrix <- table(predictions, test$LocationType)
print(confusion_matrix)

# Calculate accuracy
accuracy <- mean(predictions == test$LocationType)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))




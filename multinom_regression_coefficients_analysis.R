# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(nnet)

# Load the data from the Excel file
file_path <- "data.xlsx"
data <- read_excel(file_path)

# Convert relevant columns to factor if they are not already
data$AgeGroup <- as.factor(data$AgeGroup)
data$LocationType <- as.factor(data$LocationType)
data$PoliceArea <- as.factor(data$PoliceArea)
data$ROVDivision <- as.factor(data$ROVDivision)
data$TerritorialAuthority <- as.factor(data$TerritorialAuthority)
data$PersonOrOrganization <- as.factor(data$PersonOrOrganization)
data$AnzsocDivision <- as.factor(data$AnzsocDivision)

# Multinomial logistic regression model
multinom_model <- multinom(AnzsocDivision ~ AgeGroup + LocationType + PoliceArea + ROVDivision + TerritorialAuthority + PersonOrOrganization, data = data)

# Display model summary
summary(multinom_model)

# Extract coefficients and standard errors
coefficients <- summary(multinom_model)$coefficients
std_errors <- summary(multinom_model)$standard.errors

# Calculate z-values and p-values
z_values <- coefficients / std_errors
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Display coefficients and p-values
coefficients
p_values

# Compute AIC
AIC(multinom_model)

# Predict the AnzsocDivision categories
predicted_classes <- predict(multinom_model, newdata = data)

# Generate confusion matrix
table(data$AnzsocDivision, predicted_classes)

# Extract coefficients for visualization
coef_data <- as.data.frame(coefficients)
coef_data$Category <- rownames(coef_data)

# Reshape data for ggplot
library(tidyr)
coef_long <- coef_data %>%
  gather(key = "Variable", value = "Coefficient", -Category)

# Plot coefficients using a bar chart
ggplot(coef_long, aes(x = Variable, y = Coefficient, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Regression Coefficients", x = "Variable", y = "Coefficient")

# Save coefficients as a CSV file
write.csv(coefficients, "multinom_regression_coefficients.csv")

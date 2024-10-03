# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(forecast)
library(Metrics)

# Load the data from the Excel file
file_path <- "data.xlsx"
data <- read_excel(file_path)

# Convert the Date column to date format
data$Date <- as.Date(data$Date, format = "%y/%m/%d")

# Crime statistics by month
monthly_crime_summary <- data %>%
  group_by(month = floor_date(Date, "month")) %>%
  summarise(total_crimes = sum(Victimisations))


# Draw a time series graph
ggplot(monthly_crime_summary, aes(x = month, y = total_crimes)) +
  geom_line(color = "blue") +
  labs(title = "monthly crime summary", x = "year", y = "total crime") +
  theme_minimal()

# Time series analysis using ARIMA models


# Divide into training set and validation set
train_length <- length(monthly_crime_summary$total_crimes) - 12  # 最后12个月作为验证集
train_data <- head(monthly_crime_summary$total_crimes, train_length)
test_data <- tail(monthly_crime_summary$total_crimes, 12)

# Create time series object, training data
ts_train <- ts(train_data, start = c(year(min(monthly_crime_summary$month)), month(min(monthly_crime_summary$month))), frequency = 12)

# Fit ARIMA model
fit_arima <- auto.arima(ts_train)

# View model information
summary(fit_arima)

# Use the model to predict crime data for the last 12 months
forecast_test <- forecast(fit_arima, h = 12)

# Compare the predicted results with the actual validation set data
autoplot(forecast_test) +
  autolayer(ts(test_data, start = c(year(max(monthly_crime_summary$month)) - 1, month(max(monthly_crime_summary$month))), frequency = 12), series = "Actual", PI = FALSE) +
  labs(title = "Forecast vs Actual: Crime Trends", y = "Total Crimes") +
  theme_minimal()

# Refit the model using the full data and predict the next 12 months
ts_crime_full <- ts(monthly_crime_summary$total_crimes, start = c(year(min(monthly_crime_summary$month)), month(min(monthly_crime_summary$month))), frequency = 12)

# Refit the model
fit_arima_full <- auto.arima(ts_crime_full)

# Predict crime trends for the next 12 months
forecast_future <- forecast(fit_arima_full, h = 12)

# Draw the prediction results
autoplot(forecast_future) +
  labs(title = "Crime Forecast for the Next 12 Months", y = "Total Crimes") +
  theme_minimal()

# Calculate RMSE as an evaluation metric

rmse_value <- rmse(test_data, forecast_test$mean)
print(paste("RMSE: ", rmse_value))




# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(forecast)

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
  labs(title = "monthly crime summary", x = "month", y = "total crimes") +
  theme_minimal()

# Time series analysis using ARIMA models


# Creating a time series object
ts_crime <- ts(monthly_crime_summary$total_crimes, start = c(year(min(monthly_crime_summary$month)), month(min(monthly_crime_summary$month))), frequency = 12)

# Fitting an ARIMA model
fit_arima <- auto.arima(ts_crime)

# Fitting an ARIMA model
summary(fit_arima)

# Predict crime trends over the next 12 months
forecast_crime <- forecast(fit_arima, h = 12)
autoplot(forecast_crime)


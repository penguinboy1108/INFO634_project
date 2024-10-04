# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(nnet)
library(rpart)
library(rpart.plot) 
library(lubridate)

# Load the data from the Excel file
file_path <- "data.xlsx"
data <- read_excel(file_path)

# Convert relevant columns to factor if they are not already
data$AgeGroup <- as.factor(data$AgeGroup)
data$LocationType <- as.factor(data$LocationType)
data$ROVDivision <- as.factor(data$ROVDivision)
data$TerritorialAuthority <- as.factor(data$TerritorialAuthority)
data$PersonOrOrganization <- as.factor(data$PersonOrOrganization)
data$AnzsocDivision <- as.factor(data$AnzsocDivision)

# Assuming 'DateColumn' is the name of the date column in your dataset
# Convert the date column to Date type if it is not already
data$DateColumn <- as.Date(data$Date)

# Get the current date and calculate the date for 5 years ago
five_years_ago <- Sys.Date() - years(5)

# Filter the data for the past 5 years
recent_data <- data %>% filter(DateColumn >= five_years_ago)


# 去除缺失值
data_clean <- na.omit(data)

# 构建决策树模型
decision_tree_model <- rpart(AnzsocDivision ~ AgeGroup + LocationType + ROVDivision + TerritorialAuthority + PersonOrOrganization,
                             data = data_clean, method = "class", control = rpart.control(cp = 0.01))  # 增大 cp 可简化树
# 查看决策树模型的结构
print(decision_tree_model)

# 可视化决策树
rpart.plot(decision_tree_model, type = 3, extra = 101, fallen.leaves = TRUE, main = "Decision Tree for Crime Types")

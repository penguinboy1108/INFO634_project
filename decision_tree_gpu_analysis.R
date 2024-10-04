# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xgboost)
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

# Convert the date column to Date type if it is not already
data$DateColumn <- as.Date(data$Date)

# Get the current date and calculate the date for 5 years ago
five_years_ago <- Sys.Date() - years(5)

# Filter the data for the past 5 years
recent_data <- data %>% filter(DateColumn >= five_years_ago)

# 去除缺失值
data_clean <- na.omit(recent_data)

# 将因变量（AnzsocDivision）转换为数值编码 (XGBoost 要求标签为数值)
data_clean$AnzsocDivision <- as.numeric(as.factor(data_clean$AnzsocDivision)) - 1

# 将数据转换为矩阵格式以符合 XGBoost 的输入要求
features <- data_clean[, c("AgeGroup", "LocationType", "ROVDivision", "TerritorialAuthority", "PersonOrOrganization")]
labels <- data_clean$AnzsocDivision

# 将特征转化为数值矩阵
features_matrix <- model.matrix(~ . - 1, data = features)

# 创建 DMatrix 对象，这是 XGBoost 的专用数据格式
dtrain <- xgb.DMatrix(data = as.matrix(features_matrix), label = labels)

# 设置 XGBoost 参数以使用 GPU
params <- list(
  objective = "multi:softprob",  # 多分类任务
  num_class = length(unique(labels)),  # 类别数量
  tree_method = "gpu_hist",  # 使用 GPU
  eval_metric = "mlogloss"   # 评价指标
)

# 训练 XGBoost 模型，设定迭代次数
xgb_model <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 100,  # 训练 100 轮
  verbose = 1
)

# 查看模型的重要性特征
importance <- xgb.importance(model = xgb_model)
print(importance)

# 可视化特征重要性
xgb.plot.importance(importance_matrix = importance)
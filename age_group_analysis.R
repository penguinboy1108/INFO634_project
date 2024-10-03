# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)


# Load the data from the Excel file
file_path <- "data.xlsx"
data <- read_excel(file_path)


# 交叉表：AgeGroup 和 ANZSOC Division
age_anzsoc_table <- table(data$AgeGroup, data$AnzsocDivision)

# 查看交叉表
print(age_anzsoc_table)

# 将交叉表转换为数据框
age_anzsoc_df <- as.data.frame(age_anzsoc_table)

# 将交叉表保存为 CSV 文件
write.csv(age_anzsoc_df, file = "age_anzsoc_cross_table.csv", row.names = FALSE)

# 对交叉表进行可视化

age_anzsoc_df <- as.data.frame(age_anzsoc_table)

ggplot(age_anzsoc_df, aes(Var1, Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Crime Count", fill = "ANZSOC Division",
       title = "Crime Count by Age Group and ANZSOC Division") +
  theme_minimal()

# 交叉表：AgeGroup 和 LocationType
age_location_table <- table(data$AgeGroup, data$LocationType)

# 将交叉表转换为数据框
age_location_df <- as.data.frame(age_location_table)

# 将交叉表保存为 CSV 文件
write.csv(age_location_df, file = "age_location_cross_table.csv", row.names = FALSE)

# 查看交叉表
print(age_location_table)

# 可视化：年龄组与犯罪地点类型的关系
age_location_df <- as.data.frame(age_location_table)

ggplot(age_location_df, aes(Var1, Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Group", y = "Crime Count", fill = "Location Type",
       title = "Crime Count by Age Group and Location Type") +
  theme_minimal()

# 进行卡方检验：AgeGroup 和 ANZSOC Division
chi_age_anzsoc <- chisq.test(age_anzsoc_table)

# 输出结果
print(chi_age_anzsoc)

# 进行卡方检验：AgeGroup 和 Location Type
chi_age_location <- chisq.test(age_location_table)

# 输出结果
print(chi_age_location)


# 将交叉表转换为矩阵
age_anzsoc_matrix <- as.matrix(age_anzsoc_table)

# 生成热图

ggplot(melt(age_anzsoc_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(x = "Age Group", y = "ANZSOC Division", fill = "Crime Count",
       title = "Heatmap of Age Group vs ANZSOC Division") +
  theme_minimal()

# 将交叉表转换为矩阵
age_location_matrix <- as.matrix(age_location_table)

# 生成热图
ggplot(melt(age_location_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(x = "Age Group", y = "Location Type", fill = "Crime Count",
       title = "Heatmap of Age Group vs Location Type") +
  theme_minimal()
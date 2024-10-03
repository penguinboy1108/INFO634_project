# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggfortify)

# Load the data from the Excel file
file_path <- "data.xlsx"
data <- read_excel(file_path)

# Consolidate the data into a format with TerritorialAuthority as the primary key
region_crime_data <- data %>%
  group_by(TerritorialAuthority, AnzsocDivision) %>%
  summarise(total_crimes = sum(Victimisations),.groups = 'drop') %>%
  spread(key = AnzsocDivision, value = total_crimes, fill = 0) %>%
  ungroup()

# Use the scale function to standardize the data
scaled_data <- as.data.frame(scale(region_crime_data[, -1]))  # 除去第一列TerritorialAuthority
rownames(scaled_data) <- region_crime_data$TerritorialAuthority


# Determining the optimal number of clusters - Elbow Method
set.seed(123)  # To ensure consistency of results
wss <- sapply(1:10, function(k) {
  kmeans(scaled_data, centers = k, nstart = 10)$tot.withinss
})

# Plotting the Elbow Rule
ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method to Determine Optimal Number of Clusters", 
       x = "Number of Clusters (k)", 
       y = "Within Sum of Squares (WSS)") +
  theme_minimal()

# K-means clustering using the optimal number of clusters
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 10)

# Add the clustering results to the original data
region_crime_data$cluster <- as.factor(kmeans_result$cluster)

# view result
print(head(region_crime_data))

# Dimensionality reduction using PCA
pca_result <- prcomp(scaled_data)

# Draw a PCA scatter plot of the clusters
autoplot(pca_result, data = region_crime_data, colour = 'cluster', label = TRUE, 
         label.size = 3, main = "PCA of Crime Data by Clusters") +
  scale_color_manual(values = c("#FF6666", "#66B3FF", "#99FF99")) +
  theme_minimal()

write.csv(region_crime_data, "region_crime_clusters.csv", row.names = FALSE)
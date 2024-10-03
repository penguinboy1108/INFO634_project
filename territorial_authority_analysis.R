# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


# Load the data from the Excel file
file_path <- "data.xlsx"
data <- read_excel(file_path)

# Crime distribution by region
crime_by_region <- data %>%
  group_by(TerritorialAuthority, AnzsocDivision) %>%
  summarise(total_victimisations = sum(Victimisations)) %>%
  arrange(desc(total_victimisations))


# Calculate the proportion of crimes with unidentified offenders by region
unidentified_crimes_by_region <- data %>%
  filter(ROVDivision == "No Offender Identified") %>%
  group_by(TerritorialAuthority) %>%
  summarise(total_unidentified_victimisations = sum(Victimisations))

# Calculate total crimes by region
total_crimes_by_region <- data %>%
  group_by(TerritorialAuthority) %>%
  summarise(total_victimisations = sum(Victimisations))

# Merge the two datasets and calculate the percentage of unidentified offenders
crime_with_unidentified_percentage <- unidentified_crimes_by_region %>%
  inner_join(total_crimes_by_region, by = "TerritorialAuthority") %>%
  mutate(unidentified_percentage = (total_unidentified_victimisations / total_victimisations) * 100)

# Display the summary with the percentage of unidentified offenders
crime_with_unidentified_percentage %>%
  arrange(desc(unidentified_percentage)) 

# save as CSV 
write.csv(crime_with_unidentified_percentage, "crime_with_unidentified_percentage.csv", row.names = FALSE)


# draw unidentified percentage bar chart
ggplot(crime_with_unidentified_percentage, aes(x = reorder(TerritorialAuthority, -unidentified_percentage), y = unidentified_percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # 让条形图水平显示
  labs(title = "unidentified percentage in Territorial Authority", 
       x = "TerritorialAuthority", 
       y = "unidentified_percentage") +
  theme_minimal()


# Create a crosstab showing crime locations and crime types in different regions
crime_location_type_table <- data %>%
  group_by(TerritorialAuthority, LocationType, AnzsocDivision) %>%
  summarise(total_victimisations = sum(Victimisations)) %>%
  pivot_wider(names_from = AnzsocDivision, values_from = total_victimisations, values_fill = list(total_victimisations = 0))

# View Crosstab
head(crime_location_type_table)

# Save the crosstab as a CSV file
write.csv(crime_location_type_table, "crime_location_type_table.csv", row.names = FALSE)

# To simplify the graph, we only selected the areas with the most crimes for analysis.
top_regions <- data %>%
  group_by(TerritorialAuthority) %>%
  summarise(total_victimisations = sum(Victimisations)) %>%
  top_n(5, total_victimisations) %>%
  pull(TerritorialAuthority)

# Filter the data for the first 5 regions
filtered_data <- data %>%
  filter(TerritorialAuthority %in% top_regions)

# Create a heat map showing the relationship between crime location and crime type
ggplot(filtered_data, aes(x = LocationType, y = AnzsocDivision, fill = Victimisations)) +
  geom_tile() +
  facet_wrap(~TerritorialAuthority) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "relationship between Location Type and ANZSOC Division", 
       x = "Location Type", 
       y = "ANZSOC Division") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summarize the number of victims per age group per region
age_group_crime_summary <- data %>%
  group_by(TerritorialAuthority, AgeGroup) %>%
  summarise(total_victims = sum(Victimisations)) %>%
  arrange(desc(total_victims))

# View summary results
head(age_group_crime_summary)

# Create a bar chart showing the number of victims of different age groups in different regions
ggplot(age_group_crime_summary, aes(x = TerritorialAuthority, y = total_victims, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Victimizations targeting different age groups in different Territorial Authority", 
       x = "Territorial Authority", 
       y = "Victimizations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

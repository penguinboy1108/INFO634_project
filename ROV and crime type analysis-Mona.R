# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Load the data from the Excel file
file_path <- "data.xlsx"
data <- read_excel(file_path)

#Calculate the frequency of different offender-victim relationships
rov_distribution <- data %>%
  group_by(ROVDivision) %>%
  summarise(total_cases = sum(Victimisations)) %>%
  arrange(desc(total_cases))

# Create a bar chart showing the frequency distribution of different offender-victim relationships
ggplot(rov_distribution, aes(x = reorder(ROVDivision, -total_cases), y = total_cases, fill = ROVDivision)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency distribution of ROV", 
       x = "ROV", 
       y = "Victimisations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pie chart
ggplot(rov_distribution, aes(x = "", y = total_cases, fill = ROVDivision)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportional distribution of different ROV") +
  theme_void()

# Filter data for the specified ANZSOC Division
crime_filtered <- data %>%
  filter(ROVDivision %in% c("Known To Victim","Stranger"))

# Summarize data by crime type and ROVDivision
crime_summary <- crime_filtered %>%
  group_by(ROVDivision, AnzsocDivision) %>%
  summarise(Total_Crimes = sum(Victimisations), .groups = "drop") %>%
  ungroup()

#bar chart of victims by Crime Type with "know to victim" and "stranger"
ggplot(crime_summary, aes(x = AnzsocDivision, y = Total_Crimes, fill = ROVDivision)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Victims by Crime Type and Crime Relationship",
       x = "Crime Type",
       y = "Number of Victims",
       fill = "Relationship") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 5))




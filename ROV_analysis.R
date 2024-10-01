# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

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

# Serious crime types are specific to the ANZSOC Division
serious_crime_types <- c("Acts Intended to Cause Injury", "Robbery, Extortion and Related Offences
", "Abduction, Harassment and Other Related Offences Against a Person","Sexual Assault and Related Offences
")  

# Summarize serious crimes under different relationships
rov_risk_summary <- data %>%
  filter(AnzsocDivision %in% serious_crime_types) %>%
  group_by(ROVDivision) %>%
  summarise(total_serious_cases = sum(Victimisations)) %>%
  arrange(desc(total_serious_cases))

# View a summary of serious crimes by relationship
print(rov_risk_summary)

# Calculate the total number of cases for each relationship
total_cases_by_rov <- data %>%
  group_by(ROVDivision) %>%
  summarise(total_cases = sum(Victimisations))

# Combine serious crime cases with total cases and calculate the ratio
rov_risk_summary <- rov_risk_summary %>%
  left_join(total_cases_by_rov, by = "ROVDivision") %>%
  mutate(serious_case_ratio = (total_serious_cases / total_cases) * 100)

# View the final risk summary
print(rov_risk_summary)
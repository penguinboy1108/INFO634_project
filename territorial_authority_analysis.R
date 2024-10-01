# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

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
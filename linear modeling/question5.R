library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

# Importing population dataset
population_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/Population/Cleaned_Population_Data.csv")

# Importing the cleaned school dataset
cleaned_school_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/school/cleaned_school.csv")

# Importing the cleaned crime dataset
cleaned_crime_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/Crime/cleaned_crime_final.csv")


# Grouping school data by town and county and finding average score for each group
grouped_school_data <- cleaned_school_data %>%
  filter(Year == "2021-2022") %>%
  group_by(Town, County) %>%
  mutate(Town = tolower(Town)) %>%  # Converting the town to lowercase
  summarise(Attainment_Score = mean(Attainment_8_Score, na.rm = TRUE))

# Modifying crime dataset to show drug offence rate and crime count  
crime_dataset_drugs <- cleaned_crime_data %>%
  group_by(ShortPostcode, CrimeType, Year, Month, `Town/City`) %>%
  select(ShortPostcode, CrimeType, Year, Month,`Town/City`) %>%
  na.omit() %>%
  tally() %>%  # Creating crime count column
  rename(Crime_Count = n) %>%
  right_join(population_data, by = c("ShortPostcode" = "Short Postcode")) %>%
  select(ShortPostcode, CrimeType, Crime_Count, Population, Year, Month, `Town/City`) %>%
  na.omit() %>%
  filter(CrimeType == "Drugs") %>%  # Filtering to show only drug crimes of 2022
  mutate(Drug_Offence_Rate = (Crime_Count / Population))  # Calculating drug offence rate

View(crime_dataset_drugs)

# Grouping the drug crime dataset by county and town and showing the rate for each group for the year 2021
grouped_drug_crime <- crime_dataset_drugs %>%
  filter(Year == "2022") %>%
  group_by(`Town/City`) %>%
  mutate(`Town/City` = tolower(`Town/City`)) %>%  # Converting the town to lowercase
  summarise(Drug_Offence_Rate = mean(Drug_Offence_Rate, na.rm = TRUE))

View(grouped_drug_crime)

# Joining school data and drug crime data in a single table
school_drug <- grouped_school_data %>%
  left_join(grouped_drug_crime, by = c("Town" = "Town/City")) %>%
  na.omit()  # Removing rows with null values


# Creating a linear model 
l_model <- lm(data = school_drug, Attainment_Score ~ Drug_Offence_Rate) 

# This model predicts average attainment score as a function of drug offence rate

# Showing summary of the linear model
summary(l_model) 

# Creating the linear model graph
ggplot(school_drug, aes(x = Drug_Offence_Rate, y = Attainment_Score)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) +  # Setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  geom_smooth(method = lm, se = FALSE, color = "brown") +  # Adding linear regression line and omitting error bands 
  labs(x = "Drug Offence Rate",
       y = "Attainment Score",
       title = "2022 Attainment Score vs Drug Offence Rate",
       color = "County")  # Setting labels

# Load necessary libraries
library(tidyverse)
library(dplyr)

# Read the 2011 population data from CSV
pop_2011 = read_csv("D:/sem4/data science for developers/assingments/population/Population2011.csv", show_col_types = FALSE)

# View the original 2011 population data
View(pop_2011)

# Estimate the 2023 population by applying a growth factor to the 2011 population
pop_2023 = pop_2011 %>%
  mutate(Population = as.integer(Population * 1.00561255390388033))

# View the estimated 2023 population data
View(pop_2023)


# Assuming there's a dataset `PostcodeToLSOA` with 'ShortPostcode' and 'LSOA Code' columns
PostcodeToLSOA = read_csv("D:/sem4/data science for developers/DataScience/DataScience/Post code to lsoa/Clean_Postcode_to_LSOA.csv")
View(PostcodeToLSOA)

population_with_lsoa <- pop_2023 %>%
  # Assume the population dataset also has a column named 'Postcode' to join with
  left_join(PostcodeToLSOA, by = c("Postcode" = "ShortPostcode")) %>%
  select(`LSOA Code`, Postcode, Population) %>% 
  drop_na() %>% 
  distinct()# Remove rows with NA values

view(population_with_lsoa)


colnames(population_with_lsoa) <- c("ID", "Short Postcode", "Population")

# View the merged dataset with LSOA codes
View(population_with_lsoa)

# Save Population2023 data to a new CSV file
write.csv(population_with_lsoa, "D:/sem4/data science for developers/DataScience/DataScience/Population/Cleaned_Population_Data.csv", row.names = FALSE)

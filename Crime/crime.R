# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(dplyr)      # For data manipulation
library(lubridate)  # For working with dates
library(fmsb)       #for working with radar chart

# Set working directory
setwd("D:/sem4/data science for developers/DataScience/DataScience/Crime")

# Define the path to the main directory containing all the year-month folders
main_directory = "D:/sem4/data science for developers/assingments/crime dataset"

# Create a list of all CSV file paths in the main directory
crime_file_paths = list.files(main_directory, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

# Read and combine all CSV files into one data frame
combined_crime_data = crime_file_paths %>%
  set_names() %>%             # Ensure each element in file_paths is named
  map_df(~read_csv(.x)) %>%   # Apply read_csv to each file path
  as_tibble()                 # Convert the combined data into a tibble

# Import cleaned postcode to LSOA CSV into R
cleaned_postcode_to_lsoa = read.csv("D:/sem4/data science for developers/DataScience/DataScience/Post code to lsoa/Clean_Postcode_to_LSOA.csv", 
                                    colClasses = c("LSOA.Code" = "character"))

# Filter and rename columns in the crime data
filtered_crime <- combined_crime_data %>%
  select(`Month`, `LSOA code`, `Crime type`) %>%
  rename(Year = `Month`, LSOA.Code = `LSOA code`, CrimeType = `Crime type`)

# Process Year and Month
filtered_crime <- filtered_crime %>%
  mutate(
    Month = as.numeric(sub(".*-(\\d{2})$", "\\1", Year)),  # Extract month from Year-Month string
    Year = substr(Year, 1, 4)  # Extract year from Year-Month string
  )

write.csv(filtered_crime, "D:/sem4/data science for developers/DataScience/DataScience/Crime/Cleaned_Crime_Data_All.csv", row.names = FALSE)

# Filter crime data for the year 2022 and specific crime types
crime_data_2022 <- filtered_crime %>%
  filter(Year == "2022" & CrimeType %in% c("Drugs", "Vehicle crime", "Robbery")) %>%
  mutate(Year = as.numeric(Year))

# Load population and housing data
population_data = read.csv("D:/sem4/data science for developers/DataScience/DataScience/Population/Cleaned_Population_Data.csv")
housing_data = read.csv("D:/sem4/data science for developers/DataScience/DataScience/housing/house_selling_clean.csv")

# Merge crime data with LSOA data and remove missing values
crime_data <- crime_data_2022 %>%
  left_join(cleaned_postcode_to_lsoa, by = "LSOA.Code") %>%
  na.omit()
colnames(crime_data)
#View(crime_data)

# Merge crime data with housing data and select relevant columns
crime_with_house = crime_data %>%
  left_join(housing_data, by = "Postcode") %>%
  select(Year, LSOA.Code, CrimeType, Month, Postcode, `ShortPostcode.x`, `Town.City.x`, County.x) %>%
  rename(ShortPostcode =`ShortPostcode.x`,`Town/City` = `Town.City.x`,County = `County.x`) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  na.omit() %>%
  distinct()

View(crime_with_house)

# Save the cleaned dataset to a CSV file
write.csv(crime_with_house, "D:/sem4/data science for developers/DataScience/DataScience/Crime/cleaned_crime_final.csv", row.names = FALSE)




colnames(crime_with_house)

# Filter the data for the year 2022 and for drug offences
drug_offenses <- crime_with_house %>%
  filter(CrimeType == "Drugs" & Year == "2022")

# Group the data by County and Town/City and calculate the count of drug offenses
drug_offense_rate <- drug_offenses %>%
  group_by(County, `Town/City`) %>%
  summarise(Drug_Offense_Count = n())

# Generate the boxplot for drug offence rate in towns/districts for both counties
ggplot(drug_offense_rate, aes(x = County, y = Drug_Offense_Count, fill = County)) +
  geom_boxplot() +
  labs(title = "Drug Offence Rate in Towns/Districts for Cornwall and Bristol (2022)",
       x = "Counties",
       y = "Total Drug Offences") +
  theme_minimal() 





# Filter the data for robbery crimes in the selected month of 2022
robbery_crimes <- crime_with_house %>%
  filter(CrimeType == "Robbery" & Year == 2022 & Month == 2)

# Group by County to calculate total robbery crimes
robbery_crime_rate <- robbery_crimes %>%
  group_by(County) %>%
  summarise(Total_Robbery_Crimes = n()) %>%
  arrange(desc(Total_Robbery_Crimes))

# Prepare data for the pie chart
pie_data <- robbery_crime_rate %>%
  select(County, Total_Robbery_Crimes)

# Generate the pie chart for robbery crime rate
ggplot(pie_data, aes(x = "", y = Total_Robbery_Crimes, fill = County)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Robbery Crime Rate in February 2022") +
  theme_void() 





####################################################################
# Filter for vehicle crime offences in May
vehicle_offences_by_region <- crime_with_house %>%
  filter(Month == 5 & CrimeType == "Vehicle crime") %>%
  group_by(ShortPostcode) %>%
  mutate(TotalVehicleOffences = n()) %>%
  select(`Town/City`, District = County, TotalVehicleOffences, ShortPostcode) %>%
  distinct()

# Merge vehicle offences data with population data
vehicle_population_data <- vehicle_offences_by_region %>%
  left_join(population_data, by = c("ShortPostcode" = "Short.Postcode")) %>%
  na.omit() %>%
  group_by(ShortPostcode) %>%
  select(`Town/City`, District, Population, TotalVehicleOffences) %>%
  distinct()

# Calculate vehicle crime rate per 10,000 people
vehicle_crime_rate <- vehicle_population_data %>%
  mutate(VehicleCrimeRatePerTenThousand = (TotalVehicleOffences / Population) * 10000)


# Prepare data for radar chart
district_vehicle_crime_data <- vehicle_crime_rate %>%
  group_by(`Town/City`) %>%
  summarize(AverageVehicleCrimeRate = mean(VehicleCrimeRatePerTenThousand, na.rm = TRUE))

# Create radar data
vehicle_crime_max <- max(district_vehicle_crime_data$AverageVehicleCrimeRate, na.rm = TRUE)
vehicle_crime_min <- min(district_vehicle_crime_data$AverageVehicleCrimeRate, na.rm = TRUE)

radar_vehicle_data <- as.data.frame(rbind(
  rep(vehicle_crime_max, ncol(district_vehicle_crime_data)),  # max row
  rep(vehicle_crime_min, ncol(district_vehicle_crime_data)),  # min row
  district_vehicle_crime_data$AverageVehicleCrimeRate          # actual data
))

# Set row and column names
rownames(radar_vehicle_data) <- c("Max", "Min", "Actual Data")
colnames(radar_vehicle_data) <- as.character(district_vehicle_crime_data$`Town/City`)

# Plot the radar chart
radarchart(radar_vehicle_data, 
           axistype = 1, 
           pcol = rainbow(ncol(radar_vehicle_data)), 
           pfcol = scales::alpha(rainbow(ncol(radar_vehicle_data)), 0.3), 
           plwd = 2,
           title = "Average Vehicle Crime Rate per 10,000 by Cities/Towns in May 2022",
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "black", 
           caxislabels = seq(0, vehicle_crime_max, by = 25), 
           cglwd = 0.8,
           vlcex = 0.7) # Reduce label text size








#Filter the data for drug offenses in 2022
drug_offenses <- crime_with_house %>%
  filter(CrimeType == "Drugs" & Year == 2022)

# Group by County and Month to calculate total drug offenses
drug_offense_rate <- drug_offenses %>%
  group_by(County, Month) %>%
  summarise(Total_Drug_Offenses = n()) %>%
  arrange(Month)

# Generate the line chart for total drug offenses in both counties
ggplot(drug_offense_rate, aes(x = Month, y = Total_Drug_Offenses, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Total Drug Offenses in Both Counties (2022)",
       x = "Month", 
       y = "Total Drug Offenses") +
  theme_minimal()



















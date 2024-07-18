library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)



LSOA = read.csv("D:/sem4/data science for developers/assingments/Postcode to LSOA.csv/Postcode_to_LSOA.csv")
View(LSOA)

colnames(LSOA)
## Get the unique city names from the ladmn column
#unique_cities <- unique(LSOA$ladnm)
## Print the unique cities
#print(unique_cities)

# Filter the data to include only rows where ladmn is "Bristol" or "Cornwall"
filtered_LSOA <- LSOA %>%
  filter(ladnm %in% c("Bristol, City of", "Cornwall"))

# View the filtered data (optional)
View(filtered_LSOA)

# Define the path where you want to save the combined file
output_file_path = 'D:/sem4/data science for developers/DataScience/DataScience/LOSA/finalLosa_file.csv'


# Save the final dataframe to a new CSV file
write.csv(filtered_LSOA, output_file_path, row.names = FALSE)










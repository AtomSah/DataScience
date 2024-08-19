
library(tidyverse)
library(dplyr)


#-----------House Price Ranking-----------#

# Import cleaned population data (if needed)
cleaned_population_data <- read_csv("Cleaned Data/Cleaned Population.csv")

# Import cleaned house price data and other factors (broadband, crime, school)
cleaned_houseprices <- read_csv("Cleaned Data/Cleaned House Prices.csv")

house_score_rank <- house_selling_clean %>%
  filter(SaleYear == "2022") %>%  # Filtering by year if needed
  group_by(`Town/City`, County) %>%
  summarise(
    Avg_Price = mean(Price, na.rm = TRUE),
  ) %>%
  arrange(Avg_Price) %>%  # Sorting by Avg_Price in ascending order
  mutate(
    House_Score = 10 - (Avg_Price / 1000000),  # Lower price gets higher rank
  ) %>%
  arrange(desc(House_Score)) %>%  # Arranging towns by the highest Final_Score
  select(`Town/City`, County, Final_Score)


# Defining path to save the ranking CSV
file_path <- "D:/sem4/data science for developers/DataScience/DataScience/Recomendated town/House Ranking.csv"

# Saving the ranking CSV
write.csv(house_score_rank, file_path, row.names = FALSE)

# View the ranking table
View(house_score_rank)





#-----------Download Speed Ranking-----------#

# Importing necessary libraries
library(tidyverse)

# Importing the cleaned broadband speed data
cleaned_broadband_speed <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/BroadBand/cleaned_broadband.csv")

# Creating a new broadband speed ranking table
download_speed_rank <- cleaned_broadband_speed %>%
  group_by(`Town/City`, County) %>%
  summarise(`Average download speed (Mbit/s)` = mean(AvgDownloadspeed, na.rm = TRUE), County = first(County)) %>%
  arrange(desc(`Average download speed (Mbit/s)`)) %>%  # Arranging in descending order of download speed
  mutate(`Download Score` = (`Average download speed (Mbit/s)` / 100)) %>%  # Calculating the download score
  select(`Town/City`, County, `Download Score`) %>%
  distinct(`Town/City`, County, .keep_all = TRUE)  # Keeping only distinct rows

# Defining the path to save the download speed ranking CSV
file_path <- "D:/sem4/data science for developers/DataScience/DataScience/Recomendated town/Broadband Speed Rank.csv"

# Saving the download speed ranking to a CSV file
write.csv(download_speed_rank, file_path, row.names = FALSE)

# Viewing the download speed ranking table
View(download_speed_rank)






#-----------Crime Ranking-----------#
# Load necessary libraries
library(tidyverse)
library(dplyr)

# Import the cleaned crime dataset
crime_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/Crime/cleaned_crime_final.csv")
View(crime_data)

# Import the cleaned population dataset
population_dataset <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/Population/Cleaned_Population_Data.csv")
View(population_dataset)

# Count crimes by year, LSOA, and type
crime_dataset_count <- crime_data %>%
  group_by(ShortPostcode, CrimeType, Year, County, `Town/City`) %>%
  # Grouping by ShortPostcode, CrimeType, Year, and County
  select(ShortPostcode, CrimeType, Year, County,`Town/City`) %>%
  na.omit() %>%
  tally() %>%  # Creating the crime count column
  rename(CrimeCount = n) %>%  # Renaming for clarity
  left_join(population_dataset, by = c("ShortPostcode" = "Short Postcode")) %>%
  # Joining with population dataset using the correct column names
  select(ShortPostcode, CrimeCount, `Town/City`, County) %>%  # Selecting the required columns
  na.omit() %>% 
  distinct()

View(crime_dataset_count)

# Calculating the crime rank
crime_rank <- crime_dataset_count %>%
  rename(Town = `Town/City`) %>%  # Renaming for consistency
  group_by(Town) %>%
  summarise(MeanCrime = mean(CrimeCount), County = first(County)) %>%
  arrange(MeanCrime) %>%  # Arranging MeanCrime in ascending order
  mutate(CrimeScore = 10 - (MeanCrime / 1000)) %>%
  # Calculating the score (lower crime means higher rank)
  select(Town, County, CrimeScore)

# Define the path to save the crime rank CSV
file_path <- "D:/sem4/data science for developers/DataScience/DataScience/Recomendated town/CrimeRanking.csv"

# Save the crime rank CSV
write.csv(crime_rank, file_path, row.names = FALSE)

# View the crime rank
view(crime_rank)




#-----------School Ranking-----------#


# Import cleaned school dataset
school_dataset <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/school/cleaned_school.csv")

# Convert town and county names to upper case for consistency
school_rank <- school_dataset %>%
  mutate(Town = toupper(Town), County = toupper(County)) %>%
  group_by(Town) %>%
  mutate(`Mean Attainment` = mean(`Attainment_8_Score`, na.rm = TRUE), County = first(County)) %>%
  arrange(desc(`Mean Attainment`)) %>%
  mutate(`School Score` = (`Mean Attainment` / 10)) %>%
  select(Town, County, `School Score`) %>%
  distinct() %>% 
  na.omit()


# Define path to save school rank CSV
file_path <- "D:/sem4/data science for developers/DataScience/DataScience/Recomendated town/SchoolRanking.csv"

# Save the school rank CSV
write.csv(school_rank, file_path, row.names = FALSE)

# View the resulting school rank data
View(school_rank)



#-----------Joining all the ranking table-----------#

combined_ranking_table <-house_price_rank %>% #starting with house price rank table
  left_join(download_speed_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with download speed rank table
  na.omit() %>%
  left_join(crime_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with crime rank table
  na.omit() %>%
  left_join(school_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with school rank table
  na.omit()



#-----------Calculation of total score-----------#

final_rank <- combined_ranking_table %>%
  mutate(`Total Score` = (`House Score` + `Download Score` + `CrimeScore` + `School Score`) / 4) %>%
  #creating a new column to show the total score
  select(`Town/City`, County, `House Score`, `Download Score`, `CrimeScore`,`School Score`, `Total Score`) %>% 
  #arranging the order for columns
  arrange(desc(`Total Score`)) %>%   #showing the highest score first
  mutate(Rank= row_number()) %>%
  select(Rank, everything()) #moving the serial number column at first

#defining path to save final ranks csv
file_path <- "C:/Users/User/Desktop/220215_data_Assignment/reccomendation/FinalCombinedTownRanking.csv"

#saving the final ranks csv
write.csv(final_rank, file_path, row.names = FALSE)
view(final_rank)




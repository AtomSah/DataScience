library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)


#importing population dataset
population_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/Population/Cleaned_Population_Data.csv")

# Importing the cleaned school dataset
crime_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/Crime/cleaned_crime_final.csv")

#importing the cleaned broadband speed
broadband_speed= read_csv("D:/sem4/data science for developers/DataScience/DataScience/BroadBand/cleaned_broadband.csv")

colnames(broadband_speed)


#grouping broadband speed by town and county and finding average download speed for each group
grouped_broadband = broadband_speed %>%
  group_by(`Town/City`,County) %>%
  summarise(`AvgDownloadspeed`= mean(`AvgDownloadspeed`))
View(grouped_broadband)

colnames(population_data)


#modifyinpopulation_data#modifying our crime dataset to show drug offence rate and crime count  
crime_dataset_drugs <- crime_data %>%
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

#grouping the drug crime dataset by county and town and showing the rate for each group for the year 2022
grouped_drug_crime <- crime_dataset_drugs %>%
  filter(Year == "2022") %>%
  group_by(`Town/City`) %>%
  summarise(Drug_Offence_Rate = mean(Drug_Offence_Rate, na.rm = TRUE))

View(grouped_drug_crime)

#joining broadband data and drug crime rate data in a single table
broadband_crime_data = grouped_broadband %>% 
  left_join(grouped_drug_crime,by="Town/City") %>% 
  na.omit #removing null values
view(broadband_crime_data)

#creating a linear model 
l_model = lm(data=broadband_crime_data, `AvgDownloadspeed"`~`Drug_Offence_Rate`)
#this model predicts Average download speed as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 


#creating the linear model graph
ggplot(broadband_crime_data,aes(x=`Drug_Offence_Rate`,y=`AvgDownloadspeed"`)) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50,5))+ #setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  #setting color as blue for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="green")+ #adding linear regression line and omitting error bands 
  labs(x="Drug Offence Rate",
       y="Average Download Speed (Mbit/s)",
       title="2022 Average Download Speed vs Drug Offence Rate",color="County") #setting labels



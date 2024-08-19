# Load necessary libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load the cleaned housing and crime datasets and population dataset
housing_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/housing/house_selling_clean.csv")
crime_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/Crime/cleaned_crime_final.csv")
population_data <- read.csv("D:/sem4/data science for developers/DataScience/DataScience/Population/Cleaned_Population_Data.csv")


#importing population dataset
population_dataset <- read_csv('D:/Data Assignment/Cleaned Data/Clean_Population_Data.csv')

#importing the cleaned house prices
cleaned_houseprices= read_csv('D:/Data Assignment/Cleaned Data/Cleaned_House_Pricing_Combined.csv') 

#importing the cleaned crime dataset
cleaned_crime_dataset <- read_csv('D:/Data Assignment/Cleaned Data/Cleaned_Crime_Dataset.csv')



View(cleaned_houseprices)

#grouping house prices by town and county and finding average price for each group
grouped_house = housing_data %>%
  filter(`SaleYear`=="2022") %>%
  group_by(`Town/City`,County) %>%
  summarise(Price=mean(Price))


view(population_data)
view(crime_data)

#modifying our crime dataset to show drug offence rate and crime count  
crime_data_modified <- crime_data %>%
  group_by(ShortPostcode, CrimeType, Year, Month, `Town/City`) %>%
  select(ShortPostcode, CrimeType, Year, Month,`Town/City`) %>%
  na.omit() %>%
  tally() %>%  # Creating crime count column
  rename(Crime_Count = n) %>%
  right_join(population_data, by = c("ShortPostcode" = "Short.Postcode")) %>%
  select(ShortPostcode, CrimeType, Crime_Count, Population, Year, Month, `Town/City`) %>%
  na.omit() %>%
  filter(CrimeType == "Drugs") %>%  # Filtering to show only drug crimes of 2022
  mutate(Drug_Offence_Rate = (Crime_Count / Population))  # Calculating drug offence rate

View(crime_data_modified)

#grouping the drug crime dataset by county and town and showing the rate for each group for the year 2022
grouped_drugcrime <- crime_data_modified %>%
  filter(Year == "2022") %>%
  group_by(`Town/City`) %>%
  summarise(Drug_Offence_Rate = mean(Drug_Offence_Rate, na.rm = TRUE))


#joining house price data and drug crime rate data in a single table
houseprice_drugcrime_data = grouped_house %>% 
  left_join(grouped_drugcrime,by="Town/City") %>% 
  na.omit #removing null values


#creating a linear model 
l_model = lm(data=houseprice_drugcrime_data, Price~`Drug_Offence_Rate`) 
#this model predicts House Price as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph
ggplot(houseprice_drugcrime_data,aes(x=`Drug_Offence_Rate`,y=Price)) +
  scale_y_continuous(limits=c(0,1000000), breaks = seq(0,1000000,200000))+ #setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  #setting color as blue for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="orange")+ #adding linear regression line and omitting error bands 
  labs(x="Drug Offence Rate",
       y="Price",
       title="2022 House Prices vs Drug Offence Rate",color="County") #setting labels
















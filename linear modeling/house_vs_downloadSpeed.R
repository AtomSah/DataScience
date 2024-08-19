# Load necessary libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load the cleaned housing dataset
cleaned_housing <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/housing/house_selling_clean.csv")


# Load the broadband data (assuming it has already been cleaned and contains download speed information)
cleaned_broadband <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/BroadBand/cleaned_broadband.csv")



#importing the cleaned house prices
cleaned_houseprices= read_csv('D:/Data Assignment/Cleaned Data/Cleaned_House_Pricing_Combined.csv')

#importing the cleaned broadband speed
cleaned_broadband_speed= read_csv('D:/Data Assignment/Cleaned Data/Clean_Internet.csv')

colnames(cleaned_broadband)

#grouping house prices by town and county and finding average price for each group
grouped_housing = cleaned_housing %>%
  group_by(`Town/City`,County) %>%
  summarise(Price=mean(Price))

#grouping broadband speed by town and county and finding average download speed for each group
grouped_broadband = cleaned_broadband %>%
  group_by(`Town/City`,County) %>%
  summarise(`AvgDownloadspeed`= mean(`AvgDownloadspeed`))

#joining house price data and broadband speed data in a single table
house_broadband = grouped_housing %>% 
  left_join(grouped_broadband,by="Town/City")
View(house_broadband)

#creating a linear model 
l_model = lm(data=house_broadband, Price~`AvgDownloadspeed`) #this model predicts Price as a function of Average download speed (Mbit/s)

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph

ggplot(house_broadband, aes(x =  `AvgDownloadspeed`, y = Price)) +
  geom_point(color = "blue") + # All points in a single color
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Linear regression line without error bands
  labs(x = "Average Download Speed (Mbit/s)",
       y = "Price",
       title = "2020 House Prices vs Average Download Speed") +
  theme_minimal()

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)


# Load the cleaned school and housing datasets
school_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/school/cleaned_school.csv", show_col_types = FALSE)
housing_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/housing/house_selling_clean.csv", show_col_types = FALSE)

view(school_data)

#grouping house prices by town and county and finding average price for each group
grouped_house_data = housing_data %>%
  filter(`SaleYear`=="2022") %>%
  group_by(`Town/City`,County) %>%
  mutate(`Town/City` = tolower(`Town/City`)) %>% #converting the town from uppercase to all lowercase
  summarise(Price=mean(Price))


#grouping school data by town and county and finding average score for each group
grouped_school_data = school_data %>%
  filter(`Year`=="2021-2022") %>%
  group_by(`Town`,County) %>%
  mutate(Town= tolower(Town)) %>%  #converting the town from to all lowercase
  summarise(`Attainment Score`=mean(`Attainment_8_Score`))
view(grouped_school_data)



#joining school data and house price data in a single table
school_houseprice_final = grouped_school_data %>% 
  left_join(grouped_house_data,by=c("Town"="Town/City")) %>% 
  na.omit #removing rows with null value


#creating a linear model 
l_model = lm(data=school_houseprice_final, `Attainment Score`~Price)
#this model predicts Average attainment score as a function of Average house prices

#showing summary of the Linear Model
summary(l_model) 
options(scipen = 999)
#creating the linear model graph
ggplot(school_houseprice_final, aes(x = Price, y = `Attainment Score`)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) + # Setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  geom_smooth(method = "lm", se = FALSE, color = "black") + # Adding linear regression line and omitting error bands
  labs(x = "House Price",
       y = "Attainment Score",
       title = "2022 Attainment Score vs House Prices") + # Setting labels
  theme_minimal()


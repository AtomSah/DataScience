library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)


#importing the cleaned broadband speed
broadband_speed= read_csv("D:/sem4/data science for developers/DataScience/DataScience/BroadBand/cleaned_broadband.csv")

View(cleaned_broadband_speed)

#importing the cleaned school dataset
cleaned_school_data <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/school/cleaned_school.csv")
View(cleaned_school_dataset)

#grouping broadband speed by town and county and finding average download speed for each group
grouped_broadband = broadband_speed %>%
  group_by(`Town/City`,County) %>%
  mutate(`Town/City`= tolower(`Town/City`)) %>%  #converting the town from to all lowercase
  summarise(`AvgDownloadspeed`= mean(`AvgDownloadspeed`))
View(grouped_broadband)

#grouping school data by town and county and finding average score for each group
grouped_school_data = cleaned_school_data %>%
  filter(`Year`=="2021-2022") %>%
  group_by(`Town`,County) %>%
  mutate(Town= tolower(Town)) %>%  #converting the town from to all lowercase
  summarise(`Attainment Score`=mean(`Attainment_8_Score`))
View(grouped_school_data)

#joining broadband data and school data in a single table
broadband_attainment = grouped_broadband %>% 
  left_join(grouped_school_data,by=c("Town/City"="Town")) %>% 
  na.omit #removing rows with null value

View(broadband_attainment)

#creating a linear model 
l_model = lm(data=broadband_attainment, `AvgDownloadspeed`~`Attainment Score`) #this model predicts Average download speed as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph
ggplot(broadband_attainment,aes(x=`Attainment Score`,y=`AvgDownloadspeed`)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) + # Setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  geom_smooth(method=lm,se=FALSE,color="black")+ #adding linear regression line and omitting error bands 
  labs(x="Attainment Score",
       y="Average Download Speed (Mbit/s)",
       title="Average Download Speed vs Attainment Score",color="County") #setting labels
ggsave("D:/Data Assignment/Code/Linear Model/Avg_Download_Speed_vs_Attainment_Score.png", width = 10, height = 6)


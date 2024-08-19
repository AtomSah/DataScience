library(tidyverse)
library(dplyr)
library(lubridate)

# Importing cleaned postcode to LSOA CSV into R
PostcodeToLSOA <- read_csv("D:/sem4/data science for developers/DataScience/DataScience/Post code to lsoa/Clean_Postcode_to_LSOA.csv")
View(PostcodeToLSOA)

# Using pipe operator to clean and join data
broadband_speed <- read_csv("D:/sem4/data science for developers/assingments/broadbrand speed/201805_fixed_pc_performance.csv") %>%
  as_tibble() %>% 
  select(`Average download speed (Mbit/s)`, `Minimum download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, postcode_space) %>%  # Only selecting columns that are required 
  rename(Postcode = `postcode_space`) %>% 
  right_join(PostcodeToLSOA, by = "Postcode") %>% 
  select(`Average download speed (Mbit/s)`, `Minimum download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, Postcode, `ShortPostcode`, `Town/City`, County) %>%  # Selecting only required columns
  na.omit() %>%  # Removing rows with null values
  distinct()  # Removing duplicate rows


# Rename the columns
broadband_speed <- broadband_speed %>%
  rename(
    AvgDownloadspeed = `Average download speed (Mbit/s)`,
    MinDownloadspeed = `Minimum download speed (Mbit/s)`,
    MaxDownloadspeed = `Maximum download speed (Mbit/s)`,
    Postcode = Postcode,
    ShortPostcode = ShortPostcode,
    `Town/City` = `Town/City`,
    County = County
  )

# View the cleaned broadband speed data
View(broadband_speed)

# Save the cleaned dataset
write.csv(broadband_speed, "D:/sem4/data science for developers/DataScience/DataScience/BroadBand/cleaned_broadband.csv", row.names = FALSE)


colnames(broadband_speed)



# Filter the data for Bristol
bristol_broadband <- broadband_speed %>%
  filter(County == 'CITY OF BRISTOL')

# Generate the boxplot for average download speed by town in Bristol with different colors for each town
ggplot(bristol_broadband, aes(x = `Town/City`, y = AvgDownloadspeed, fill = `Town/City`)) +
  geom_boxplot() +
  labs(title = "Average Download Speed (Mbit/s) for Bristol Towns", 
       x = "Town/City", 
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter the data for Cornwall
cornwall_broadband <- broadband_speed %>%
  filter(County == 'CORNWALL')

# Generate the boxplot for average download speed by town in Cornwall with different colors for each town
ggplot(cornwall_broadband, aes(x = `Town/City`, y = AvgDownloadspeed, fill = `Town/City`)) +
  geom_boxplot() +
  labs(title = "Average Download Speed (Mbit/s) for Cornwall Towns", 
       x = "Town/City", 
       y = "AvgDownloadspeed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Filter the data for Bristol
filtered_broadband <- broadband_speed %>%
  filter(County == 'Bristol, City of')

# Calculate the average and maximum download speeds for Bristol
bristol_speeds_summary <- filtered_broadband %>%
  summarise(
    Average_Download = mean(AvgDownloadspeed, na.rm = TRUE),
    Maximum_Download = mean(MaxDownloadspeed, na.rm = TRUE)
  )

# Transform the summary data for plotting
bristol_speeds_summary_long <- bristol_speeds_summary %>%
  pivot_longer(cols = c(Average_Download, Maximum_Download), 
               names_to = "Speed_Type", 
               values_to = "Speed")

# Bar chart for average and maximum download speeds
ggplot(bristol_speeds_summary_long, aes(x = Speed_Type, y = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average and Maximum Download Speeds in Bristol",
       x = "Speed Type", 
       y = "Download Speed (Mbit/s)") +
  theme_minimal() +
  scale_fill_manual(values = c("Average_Download" = "#F8766D", "Maximum_Download" = "#00BFC4"))




# Filter the data for Bristol and Cornwall
filtered_broadband <- broadband_speed %>%
  filter(County =='cornwall')



# Calculate the average and maximum download speeds for Bristol
cornwall_speeds_summary <- filtered_broadband %>%
  summarise(
    Average_Download = mean(AvgDownloadspeed, na.rm = TRUE),
    Maximum_Download = mean(MaxDownloadspeed, na.rm = TRUE)
  )

# Bar chart for average and maximum download speed for Bristol
cornwall_speeds_summary <- cornwall_speeds_summary %>%
  pivot_longer(cols = c(Average_Download, Maximum_Download), 
               names_to = "Speed_Type", 
               values_to = "Speed")

ggplot(cornwall_speeds_summary, aes(x = Speed_Type, y = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average and Maximum Download Speeds in cornwall",
       x = "Speed Type", 
       y = "Download Speed (Mbit/s)") +
  theme_minimal() +
  scale_fill_manual(values = c("Average_Download" = "#F8766D", "Maximum_Download" = "#00BFC4"))


# Bar Chart: Average and Maximum Download Speeds in Bristol
broadband_speed %>%
  filter(County == "CITY OF BRISTOL") %>%  # Filtering for Bristol
  summarize(Average_Speed = mean(`AvgDownloadspeed`),
            Maximum_Speed = max(`MaxDownloadspeed`)) %>% 
  pivot_longer(cols = c(Average_Speed, Maximum_Speed), names_to = "Speed_Type", values_to = "Speed") %>%  # Reshaping data for bar chart
  ggplot(aes(x = Speed_Type, y = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Average and Maximum Download Speeds in Bristol",
       x = "Speed Type",
       y = "Speed (Mbit/s)") +
  theme_minimal()


colnames(broadband_speed)

# Bar Chart: Average and Maximum Download Speeds in Cornwall
broadband_speed %>%
  filter(County == "CORNWALL") %>%  # Filtering for Cornwall
  summarize(Average_Speed = mean(`AvgDownloadspeed`),
            Maximum_Speed = max(`MaxDownloadspeed`)) %>% 
  pivot_longer(cols = c(Average_Speed, Maximum_Speed), names_to = "Speed_Type", values_to = "Speed") %>%  # Reshaping data for bar chart
  ggplot(aes(x = Speed_Type, y = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Average and Maximum Download Speeds in Cornwall",
       x = "Speed Type",
       y = "Speed (Mbit/s)") +
  theme_minimal()






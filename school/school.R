# Load necessary libraries
library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
library(data.table)

# Read and clean Bristol datasets
Bristol <-  read.csv("D:/sem4/data science for developers/assingments/school/bristol/2021-2022/801_ks4final.csv", fill = TRUE)

# Read and clean Cornwall datasets
Cornwall <- read.csv("D:/sem4/data science for developers/assingments/school/cornwall/2021-2022/908_ks4final.csv", fill = TRUE) 

# Bristol School Data for 2021-2022
# Selecting relevant columns and filtering out invalid data, adding Year and County columns
bristol_ks4_final_2021_22 <- Bristol %>%
  select(PCODE, SCHNAME, TOWN, URN, TOTATT8, ATT8SCR) %>%
  mutate(Year = "2021-2022") %>%
  mutate(County = "Bristol") %>%
  na.omit() %>% # Remove rows with missing values
  filter(!(TOTATT8 == "NE" | ATT8SCR == "NE" | TOTATT8 == "SUPP" | ATT8SCR == "SUPP")) # Exclude invalid entries

# Renaming columns to more descriptive names
colnames(bristol_ks4_final_2021_22) <- c(
  "Postcode", "SchoolName", "Town", "UniqueRefNo", 
  "Total_Attainment_8_Score", "Attainment_8_Score", "Year", "County"
)

# Cornwall School Data for 2021-2022
# Performing similar operations as above
cornwall_ks4_final_2021_22 <- Cornwall %>%
  select(PCODE, SCHNAME, TOWN, URN, TOTATT8, ATT8SCR) %>%
  mutate(Year = "2021-2022") %>%
  mutate(County = "Cornwall") %>%
  na.omit() %>%
  filter(!(TOTATT8 == "NE" | ATT8SCR == "NE" | TOTATT8 == "SUPP" | ATT8SCR == "SUPP"))

# Renaming columns for consistency
colnames(cornwall_ks4_final_2021_22) <- c(
  "Postcode", "SchoolName", "Town", "UniqueRefNo", 
  "Total_Attainment_8_Score", "Attainment_8_Score", "Year", "County"
)


# Combining all datasets into one
combined_ks4_final <- bind_rows(
  bristol_ks4_final_2021_22, cornwall_ks4_final_2021_22
  )

# Display the combined dataset
View(combined_ks4_final)

# Export the combined dataset to a CSV file
write.csv(combined_ks4_final, "D:/sem4/data science for developers/DataScience/DataScience/school/cleaned_school.csv", row.names=FALSE)





# Filter data for Bristol
bristol_data <- combined_ks4_final %>%
  filter(County == "Bristol")

# Create a boxplot for Bristol towns
boxplot_bristol <- ggplot(bristol_data, aes(x = Town, y = Attainment_8_Score, fill = Town)) +
  geom_boxplot() +
  labs(
    title = "Attainment 8 Score by Town in Bristol (2021-2022)",
    x = "Town",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the boxplot for Bristol towns
print(boxplot_bristol)

# Filter data for Cornwall
cornwall_data <- combined_ks4_final %>%
  filter(County == "Cornwall")

# Create a boxplot for Cornwall towns
boxplot_cornwall <- ggplot(cornwall_data, aes(x = Town, y = Attainment_8_Score, fill = Town)) +
  geom_boxplot() +
  labs(
    title = "Attainment 8 Score by Town in Cornwall (2021-2022)",
    x = "Town",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the boxplot for Cornwall towns
print(boxplot_cornwall)



library(ggplot2)
library(dplyr)

# Filter data for Bristol
bristol_data <- combined_ks4_final %>%
  filter(County == "Bristol")

# Calculate the average Attainment 8 score for each school
average_attainment8_bristol <- bristol_data %>%
  group_by(SchoolName) %>%
  summarise(Avg_Attainment_8_Score = mean(Attainment_8_Score, na.rm = TRUE)) %>%
  na.omit()  # Remove any rows with NA values

# Ensure that the Attainment 8 scores are numeric
average_attainment8_bristol$Avg_Attainment_8_Score <- as.numeric(average_attainment8_bristol$Avg_Attainment_8_Score)

# Create a line chart for average Attainment 8 score in Bristol schools
line_chart_bristol <- ggplot(average_attainment8_bristol, aes(x = SchoolName, y = Avg_Attainment_8_Score, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Attainment 8 Score in Bristol Schools (2021-2022)", 
       x = "School", y = "Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display the line chart for Bristol schools
print(line_chart_bristol)


colnames(combined_ks4_final)

# Filter the data for the academic year 2021-2022 and for Bristol schools only
bristol_schooldata_2021_2022 <- combined_ks4_final %>%
  filter(County == "Bristol")

# Generate the line chart for Attainment 8 score for each Bristol school
ggplot(bristol_schooldata_2021_2022, aes(x = SchoolName, y = Attainment_8_Score, group = SchoolName)) +
  geom_line(aes(color = SchoolName)) +
  geom_point() +
  labs(title = "Bristol Schools' Average Attainment 8 Score (2021-2022 Academic Year)",
       x = "School Name", 
       y = "Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_discrete(name = "School Name")


# Filter the data for the academic year 2021-2022 and for Cornwall schools only
bristol_schooldata_2021_2022 <- combined_ks4_final %>%
  filter(County == "Cornwall")

# Generate the line chart for Attainment 8 score for each Bristol school
ggplot(bristol_schooldata_2021_2022, aes(x = SchoolName, y = Attainment_8_Score, group = SchoolName)) +
  geom_line(aes(color = SchoolName)) +
  geom_point() +
  labs(title = "Cornwall Schools' Average Attainment 8 Score (2021-2022 Academic Year)",
       x = "School Name", 
       y = "Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

















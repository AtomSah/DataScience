# Load necessary libraries
library(tidyverse)
library(dplyr)

# Read housing data from CSV files for each year
house_sales_2020 = read_csv("D:/sem4/data science for developers/assingments/housing/pp-2020.csv")
house_sales_2021 = read_csv("D:/sem4/data science for developers/assingments/housing/pp-2021.csv")
house_sales_2022 = read_csv("D:/sem4/data science for developers/assingments/housing/pp-2022.csv")
house_sales_2023 = read_csv("D:/sem4/data science for developers/assingments/housing/pp-2023.csv")

# Rename columns to have consistent column names across all data frames
colnames(house_sales_2020) = colnames(house_sales_2021) = colnames(house_sales_2022) = colnames(house_sales_2023) =  c(
  "ID", "Price", "SaleDate", "Postcode", "Property_Type", "Old_New", "Durations", "PAON", "SOAN", "Street_Name", 
  "Locality", "Town/City", "District", "County", "Category_PPD", "ActiveStatus"
)

# Combine all the yearly data frames into one data frame using pipelines
house_selling_clean = bind_rows(house_sales_2020, house_sales_2021, house_sales_2022, house_sales_2023) %>%
  # Subset to keep only the desired columns
  select(ID, Postcode, Price, SaleDate, `Town/City`, County) %>%
  # Remove rows with any missing values
  na.omit() %>% 
  # Remove duplicate rows
  distinct() %>%
  # Filter the data for specific counties
  filter(County == 'CITY OF BRISTOL' | County == 'CORNWALL') %>%
  # Convert SaleDate to Date format and extract the year
  mutate(SaleDate = as.Date(SaleDate, format = "%Y-%m-%d"),
         SaleYear = format(SaleDate, "%Y"),
         # Extract and format ShortPostcode
         ShortPostcode = sub("(.{3,4})\\s*(.).*", "\\1 \\2", Postcode)) %>%
  # Select only required columns
  select(Price, SaleYear, Postcode, ShortPostcode, `Town/City`, County)

# Convert the cleaned data frame to a tibble for better display and manipulation
house_selling_clean = as_tibble(house_selling_clean)

# View the cleaned data
View(house_selling_clean)

# Write the cleaned data to a CSV file
write.csv(house_selling_clean, "D:/sem4/data science for developers/DataScience/DataScience/housing/house_selling_clean.csv", row.names = FALSE)



#data of 2022 for data analysis
house_clean_2022 = house_sales_2022 %>%
  # Subset to keep only the desired columns
  select(ID, Postcode, Price, SaleDate, `Town/City`, County) %>%
  # Remove rows with any missing values
  na.omit() %>% 
  # Remove duplicate rows
  distinct() %>%
  # Filter the data for specific counties
  filter(County == 'CITY OF BRISTOL' | County == 'CORNWALL') %>%
  # Convert SaleDate to Date format and extract the year
  mutate(SaleDate = as.Date(SaleDate, format = "%Y-%m-%d"),
         SaleYear = format(SaleDate, "%Y"),
         # Extract and format ShortPostcode
         ShortPostcode = sub("(.{3,4})\\s*(.).*", "\\1 \\2", Postcode)) %>%
  # Select only required columns
  select(Price, SaleYear, Postcode, ShortPostcode, `Town/City`, County)

# Convert the cleaned data frame to a tibble for better display and manipulation
house_clean_2022 = as_tibble(house_clean_2022)

# View the cleaned data
View(house_clean_2022)
  


# Filter data for the City of Bristol
bristol_data_2022 <- house_clean_2022 %>%
  filter(County == "CITY OF BRISTOL")
view(bristol_data_2022)

# Create a boxplot for the average house prices in 2022 for different towns in Bristol
boxplot_bristol_towns_2022 <- ggplot(bristol_data_2022, aes(x = `Town/City`, y = Price, fill = `Town/City`)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as currency
  labs(
    title = "Average House Price in 2022 (Boxplot) – Bristol's Towns",
    x = "Town/City",
    y = "House Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the boxplot for Bristol's towns
print(boxplot_bristol_towns_2022)




# Filter data for Cornwall
cornwall_data_2022 <- house_clean_2022 %>%
  filter(County == "CORNWALL")

# Create a boxplot for the average house prices in 2022 for different towns in Cornwall
boxplot_cornwall_towns_2022 <- ggplot(cornwall_data_2022, aes(x = `Town/City`, y = Price, fill = `Town/City`)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as currency
  labs(
    title = "Average House Price in 2022 (Boxplot) – Cornwall's Towns",
    x = "Town/City",
    y = "House Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the boxplot for Cornwall's towns
print(boxplot_cornwall_towns_2022)







# Filter data for the City of Bristol
bristol_data_2022 <- house_clean_2022 %>%
  filter(County == "CITY OF BRISTOL")

# Calculate average price by Town/City for Bristol
average_prices_bristol <- bristol_data_2022 %>%
  group_by(`Town/City`) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE))

# Create a bar chart for average house prices in Bristol
bar_chart_bristol_towns_2022 <- ggplot(average_prices_bristol, aes(x = `Town/City`, y = Avg_Price, fill = `Town/City`)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as currency
  labs(
    title = "Average House Price in 2022 – Bristol's Towns",
    x = "Town/City",
    y = "Average House Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the bar chart for Bristol's towns
print(bar_chart_bristol_towns_2022)



# Filter data for Cornwall
cornwall_data_2022 <- house_clean_2022 %>%
  filter(County == "CORNWALL")

# Calculate average price by Town/City for Cornwall
average_prices_cornwall <- cornwall_data_2022 %>%
  group_by(`Town/City`) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE))

# Create a bar chart for average house prices in Cornwall
bar_chart_cornwall_towns_2022 <- ggplot(average_prices_cornwall, aes(x = `Town/City`, y = Avg_Price, fill = `Town/City`)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as currency
  labs(
    title = "Average House Price in 2022 – Cornwall's Towns",
    x = "Town/City",
    y = "Average House Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the bar chart for Cornwall's towns
print(bar_chart_cornwall_towns_2022)



# Calculate average house price by year and county
average_price_by_year <- house_selling_clean %>%
  group_by(SaleYear, County) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>%
  ungroup()

# Create the line chart
line_chart_avg_price <- ggplot(average_price_by_year, aes(x = SaleYear, y = Avg_Price, color = County, group = County)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as currency
  labs(
    title = "Average House Price from 2020 to 2023",
    x = "Year",
    y = "Average House Price"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display the line chart
print(line_chart_avg_price)




















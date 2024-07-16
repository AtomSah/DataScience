library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

colm_names = c('Category', 'Price', 'Date', 'PostCode', 'null1','null2','null3','POAN','SOAN','Street','Locality','Town','District','County','null4','null5')

housing20 = read.csv("D:/sem4/data science for developers/assingments/housing/pp-2020.csv", col.names = colm_names )
housing23 = read.csv("D:/sem4/data science for developers/assingments/housing/pp-2023.csv", col.names = colm_names)
housing22 = read.csv("D:/sem4/data science for developers/assingments/housing/pp-2022.csv", col.names = colm_names)
housing21 = read.csv("D:/sem4/data science for developers/assingments/housing/pp-2021.csv", col.names = colm_names)

#View(housing20)
#View(housing21)
#View(housing22)
#View(housing23)


# Merge the datasets
merged_housing <- bind_rows(housing20, housing21, housing22, housing23)

# View the merged dataset
#View(merged_housing)

# Remove the specified columns
columns_to_remove = c('null1', 'null2', 'null3', 'null4', 'null5', 'Category','SOAN')
final_data = merged_housing %>% select(-one_of(columns_to_remove))


# View the updated dataset
#View(final_data)

write.csv(final_data, "D:/sem4/data science for developers/assingments/housing/mergehouse.csv", row.names = FALSE)

# Keep data that contains county Bristol and Cornwall using grepl
filtered_data = final_data %>%
  filter(grepl("Bristol|Cornwall", County, ignore.case = TRUE))


#View(filtered_data)

write.csv(filtered_data, "D:/sem4/data science for developers/assingments/housing/filter.csv", row.names = FALSE)

# Remove rows with too many missing values
# Set threshold for maximum allowable NA values in a row (e.g., 50% missing)
threshold = ncol(filtered_data) / 2
filtered_data = filtered_data[rowSums(is.na(filtered_data)) <= threshold, ]

# Remove columns with too many missing values
# Set threshold for maximum allowable NA values in a column (e.g., 50% missing)
threshold_col = nrow(filtered_data) / 2
filtered_data = filtered_data %>% select_if(~sum(is.na(.)) <= threshold_col)

# Impute remaining missing values
# Example: Replace missing values in numeric columns with median
filtered_data = filtered_data %>% mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))


# Remove duplicates
filtered_data = filtered_data %>% distinct()


# Remove the time component from the Date column
filtered_data = filtered_data %>% mutate(Date = as.Date(Date))


average_price <- mean(filtered_data$Price, na.rm = TRUE)
print(average_price)


# Impute missing values using the mode while excluding NA values
mode_function <- function(x) {
  ux <- unique(na.omit(x))  # Get unique values excluding NA
  tab <- tabulate(match(x, ux))  # Tabulate frequencies for non-NA values
  ux[tab == max(tab)]  # Return mode(s) for non-NA values
}
mode_function(filtered_data$PostCode)
mode_function(filtered_data$Street)
mode_function(filtered_data$Locality)

# Handle missing values, convert data types, and convert columns to uppercase
cleaned_data <- filtered_data %>%
  mutate(
    PostCode = na_if(PostCode, ""),
    Street = na_if(Street, ""),
    Locality = na_if(Locality, "")
  ) %>%
  mutate(
    PostCode = ifelse(is.na(PostCode), mode_function(PostCode), PostCode),
    Street = ifelse(is.na(Street), mode_function(Street), Street),
    Locality = ifelse(is.na(Locality), mode_function(Locality), Locality),
  ) %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Price = as.numeric(Price)
  ) %>%
  mutate(
    across(c(PostCode, Price, Date, POAN, Street, Locality, Town, District, County), toupper)
  )

# View the updated cleaned_data
view(cleaned_data)



# Summarize the average prices by town
average_prices = filtered_data %>%
  group_by(Town) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE))


Q1 <- quantile(cleaned_data$Price, 0.25, na.rm = TRUE)
Q3 <- quantile(cleaned_data$Price, 0.75, na.rm = TRUE)

Q1
Q3

IQR = Q3-Q1
IQR

outlier_threshold <- 1.5 * IQR
outlier_threshold

cleaned_data <- cleaned_data %>%
  filter(Price >= (Q1 - outlier_threshold) & Price <= (Q3 + outlier_threshold))

view(cleaned_data)


# Define the path where you want to save the combined file
output_file_path = 'D:/sem4/data science for developers/DataScience/DataScience/finalHousing_file.csv'


# Save the final dataframe to a new CSV file
write.csv(cleaned_data, output_file_path, row.names = FALSE)

ggplot(cleaned_data, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) + # Adds commas for better readability
  labs(title = "Comparison of Housing Prices: Bristol vs Cornwall",
       x = "County",
       y = "Price") +
  theme_minimal()



















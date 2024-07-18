
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)


# Read in the CSV files
broadband1 <- read.csv("D:/sem4/data science for developers/assingments/broadbrand speed/201805_fixed_pc_performance.csv")
broadband2 <- read.csv("D:/sem4/data science for developers/assingments/broadbrand speed/201809_fixed_pc_coverage.csv")


# View the data (optional)
View(broadband1)
View(broadband2)


# Perform the inner join
combined_data <- broadband1 %>%
  inner_join(broadband2, by = c("postcode_space" = "pcds"))

# View the combined data (optional)
View(combined_data)

# Perform the inner join
combined_data <- filtered_LSOA %>%
  inner_join(broadband1, by = c("pcds" = "postcode_space"))

# View the combined data (optional)
View(combined_data)


print(colnames(combined_data))

#combined_data <- combined_data %>%
# rename(post_code = pcds, County = ladnm)


# Example: Deleting columns "column1", "column2", and "column3"
cleaned_data <- combined_data %>%
  select(-c(pcd7, pcd8,doterm, usertype, oa11cd, lsoa11cd,msoa11cd,ladcd, lsoa11nm, msoa11nm, ladnmw,dointr, postcode.area ,
         postcode))



# View the cleaned data (optional)
View(cleaned_data)






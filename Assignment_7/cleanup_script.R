library(dplyr)
library(tidyverse)
library(stringr)

# Import the csv file table delimited
file = "Assignment_6/data/Dry_Eye_Dataset.csv"
df = read.delim(file, header =TRUE, sep=",")

# Explore the structure of the data 
summary(df)
count(df,is.na(df))
# No missing values

# Replace the dot to underscore in the column names 
column_names <- colnames(df)
column_names <- str_replace_all(column_names, "\\.", "_")

# Update the dataframe with the new column names
colnames(df) = column_names

# Separate the blood pressure column into sys and dys
df_updated <-  df %>% separate_wider_delim(Blood_pressure, delim = "/", names = c("Pressure_Sys", "Pressure_Dys"))

# Convert Pressure columns to numeric 
df_updated <- df_updated %>%
  mutate(across( c("Pressure_Sys", "Pressure_Dys"), as.numeric))

# For columns with Y/N responses (character columns) except Gender) replace Y with 1 and N with 0 
df_final <- df_updated %>% 
  mutate(across(where(is.character) & !any_of("Gender"),
                ~ifelse(.x == "Y", 1,0)))

# Sanity check
summary(df_final)

# Save a RDS object to be called in a separate script for linear models
saveRDS(df_final, "Assignment_6/data/clean_dry_eye_dataset.rds")



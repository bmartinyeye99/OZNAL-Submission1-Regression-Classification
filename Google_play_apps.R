library(tidyverse)
library(magrittr) # Implements pipes 
library(data.table) # Implements the %like% operator
library(caret)
library(ggplot2)
library(readxl)

setwd("D:\\MRc\\FIIT\\ING\\sem2\\oznal\\zadanie_1") 
list.files() # List all files in the working directory

data <- read.csv("googleplaystore.csv")
data_user_reviews <- read.csv("googleplaystore_user_reviews.csv")

head(data,10)
head(data_user_reviews,10)

# View(data)
View(data_user_reviews)

# Descriptive statistics
summary(data)
summary(data_user_reviews)

# Unique values
lapply(data, unique)
lapply(data_user_reviews, unique)

# Number of missing values
missing_values_data <- colSums(is.na(data))
missing_values_reviews <- colSums(is.na(data_user_reviews))

# Create a list combining missing values counts for both dataframes
miscols <- c(missing_values_data, missing_values_reviews)

# Print missing values
for (x in 1:2) {
  cat("\n Columns with corresponding number of missing values:\n")
  for (col in names(miscols)) {
    cat(sprintf("%s: %d\n", col, miscols[col]))
  }
} 


# Number of duplicate records
sum(duplicated(data))
sum(duplicated(data_user_reviews))

data <- distinct(data)
data_user_reviews <- distinct(data_user_reviews)

# data hold columns that hold numeric data in non numeric format
# these columns with be converted into numeric, and additional characters will be removed
convert_to_numeric <- function(data) {
  # Make a copy of the original dataset
  new_data <- data
  
  # Convert Reviews to numeric
  new_data$Reviews <- as.numeric(new_data$Reviews)
  
  # Convert Installs to numeric and remove '+'
  new_data$Installs <- as.numeric(gsub("[^0-9.]", "", new_data$Installs))
  
  # Convert Size to numeric and remove 'M'
  new_data$Size <- as.numeric(gsub("M", "", new_data$Size))
  
  # Convert Price to numeric and remove '$'
  new_data$Price <- as.numeric(gsub("\\$", "", new_data$Price))
  
  # Round numeric columns to 2 decimal places
  numeric_cols <- sapply(new_data, is.numeric)
  new_data[, numeric_cols] <- round(new_data[, numeric_cols], 2)
  
  # Rename columns
  colnames(new_data)[colnames(new_data) == "Size"] <- "Size_in_MB"
  colnames(new_data)[colnames(new_data) == "Price"] <- "Price_in_Dollars"
  
  return(new_data)
}

# Apply the function to the dataset
new_data <- convert_to_numeric(data)
head(new_data)
View(new_data)

# Function to fill missing values of numeric columns with mean
fill_missing_with_mean <- function(data) {
  # Find numeric columns
  numeric_cols <- sapply(data, is.numeric)
  
  # Fill missing values with mean for numeric columns
  for (col in names(data)[numeric_cols]) {
    mean_val <- mean(data[[col]], na.rm = TRUE)  # Calculate mean
    data[[col]] <- ifelse(is.na(data[[col]]), round(mean_val, 2), data[[col]])  # Replace NA with mean rounded to 2 decimal places
  }
  
  return(data)
}

# Apply the function to the dataset
new_data_filled <- fill_missing_with_mean(new_data)



# Print first few rows to verify the changes
head(new_data_filled)
View(new_data_filled)



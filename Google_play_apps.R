library(tidyverse)
library(magrittr) # Implements pipes 
library(data.table) # Implements the %like% operator
library(caret)
library(ggplot2)
library(readxl)
library(reshape2) 
library(gridExtra)


setwd("D:\\MRc\\FIIT\\ING\\sem2\\oznal\\zadanie_1") 
list.files() # List all files in the working directory

data <- read.csv("googleplaystore.csv")
data_user_reviews <- read.csv("googleplaystore_user_reviews.csv")

head(data,10)
head(data_user_reviews,10)

View(data)
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
  
  # Function to extract the substring before the second dot and convert to float
  extract_float <- function(x) {
    dots <- gregexpr("\\.", x)[[1]]
    if (length(dots) >= 2) {
      float_value <- as.numeric(substring(x, 1, dots[2] - 1))
    } else {
      float_value <- as.numeric(x)
    }
    return(float_value)
  }
  
  # Convert Current.Ver to float
  new_data$Current.Ver <- sapply(new_data$Current.Ver, extract_float)
  
  # Convert Reviews to numeric
  new_data$Reviews <- as.numeric(as.character(new_data$Reviews))
  
  # Convert Installs to numeric and remove '+'
  new_data$Installs <- as.integer(gsub("[^0-9.]", "", as.character(new_data$Installs)))
  
  # Convert Size to numeric and remove 'M'
  new_data$Size <- as.numeric(gsub("M", "", as.character(new_data$Size)))
  
  # Convert Price to numeric and remove '$'
  new_data$Price <- as.numeric(gsub("\\$", "", as.character(new_data$Price)))
  
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

handle_outliers <- function(data, columns) {
  # Loop through each specified column
  for (col in columns) {
    # Calculate quartiles
    q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    
    # Calculate IQR
    iqr <- q3 - q1
    
    # Define upper and lower bounds for outliers
    upper_bound <- q3 + 1.5 * iqr
    lower_bound <- q1 - 1.5 * iqr
    
    # Replace outliers with NA using ifelse
    data[[col]] <- ifelse(data[[col]] < lower_bound | data[[col]] > upper_bound, NA, data[[col]])
  }
  
  return(data)
}
columns_of_interest <- c("Reviews","Rating", "Size_in_MB", "Installs", "Price_in_Dollars", "Current.Ver")

# Apply the function to the dataset
#new_data_filled <- handle_outliers(new_data, columns_of_interest)
new_data_filled <- fill_missing_with_mean(new_data)

# Print first few rows to verify the changes
head(new_data_filled)
View(new_data_filled)

colnames(new_data_filled)




###### PLOTTING AND EAMINIG RELATIONS ######
new_data_filled$Rating_Level <- ifelse(new_data_filled$Rating >= 4.0, "High", "Low")

# Calculate correlation matrix
correlation_matrix <- cor(new_data_filled[, c("Rating", "Reviews", "Size_in_MB", "Installs", "Price_in_Dollars")])

# Print correlation matrix
print(correlation_matrix)

# Create a dataframe from the correlation matrix
correlation_df <- as.data.frame(correlation_matrix)
correlation_df$row <- rownames(correlation_matrix)

# Melt the dataframe to long format
correlation_df_long <- reshape2::melt(correlation_df, id.vars = "row")

# Plot heatmap
ggplot(correlation_df_long, aes(x = row, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Add a new column "Rating Level" based on the Rating column

ggplot(new_data_filled, aes(x = Rating_Level, fill = Rating_Level)) +
  geom_bar() +
  labs(title = "Distribution of Rating Levels", x = "Rating Level", y = "Count") +
  theme_minimal()

rating_counts <- table(new_data_filled$Rating_Level)
print(rating_counts)

# Distribution of Rating level and number of installs
ggplot(data = new_data_filled, aes(x = Rating_Level, y = Installs,
                        fill = factor(Rating_Level))) +
  geom_boxplot() +
  theme_minimal() +
  labs(fill = "Rating_level")


# Define the columns of interest
columns_of_interest <- c("Reviews","Rating", "Size_in_MB", "Installs", "Price_in_Dollars", "Current.Ver")


# Create scatter plots
scatter_plots <- lapply(columns_of_interest, function(col) {
  ggplot(new_data_filled, aes_string(x = col, y = "Rating")) +
    geom_point(color = 'red') +
    labs(title = paste("Scatter plot of", col, "vs Rating"), x = col, y = "Rating") +
    theme_light()
})

# Arrange scatter plots on one canvas
grid.arrange(grobs = scatter_plots, ncol = 2)  # C

# Create a column indicating whether Installs are above or below the mean
new_data_filled$Above_Mean <- ifelse(new_data_filled$Installs > mean_installs, "Above Mean", "Below Mean")

ggplot(new_data_filled, aes(x = Installs, y = 0, color = Above_Mean)) +
  geom_jitter(height = 0, width = 50000) +  # Adjust width for better visualization
  geom_vline(xintercept = mean_installs, linetype = "dashed", color = "green", linewidth = 1.5) +  # Add vertical line for mean
  labs(title = "Distribution of Installs Above and Below Mean", x = "Installs", y = "") +
  scale_color_manual(values = c("red", "blue"), guide = FALSE) +  # Set colors and remove legend
  theme_minimal()




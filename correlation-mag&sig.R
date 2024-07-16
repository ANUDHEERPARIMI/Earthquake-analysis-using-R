# Load required libraries
library(data.table)
library(dplyr)
install.packages('ggplot2')
library(ggplot2)

# Define the clean_df function
clean_df <- function(df) {
  # Getting only the useful columns
  df_clean <- df[, c('type', 'time', 'coordinates', 'mag', 'place', 'status', 'tsunami', 'sig', 
                     'net', 'nst', 'dmin', 'rms', 'gap', 'magType'), drop = FALSE]
  
  # Remove square brackets from coordinates column
  df_clean$coordinates <- gsub("\\[|\\]", "", df_clean$coordinates)
  
  # Split coordinates into separate columns
  coordinates_split <- strsplit(df_clean$coordinates, ",")
  coordinates_numeric <- sapply(coordinates_split, as.numeric)
  
  # Extract longitude, latitude, and depth
  long <- coordinates_numeric[1, ]
  lat <- coordinates_numeric[2, ]
  dep <- coordinates_numeric[3, ]
  
  df_clean$longitude <- long
  df_clean$latitude <- lat
  df_clean$depth <- dep
  
  # Fixing time
  df_clean$time <- as.POSIXct(as.numeric(df_clean$time)/1000, origin="1970-01-01")
  
  # Dropping useless coordinate column
  df_clean$coordinates <- NULL
  
  return(df_clean)
}

# Path to the directory containing CSV files
path <- '/Users/parimianudheer/Desktop/Project 1/earthquake_time_series_LSTM/datasets/imported_usgs_api'
my_files <- list.files(path, pattern = "raw.csv", full.names = TRUE)

# Combine all CSV files into a single data frame
df_combined <- rbindlist(lapply(my_files, fread))

# Remove row names
rownames(df_combined) <- NULL

# Clean the combined data frame
df_combined_clean <- clean_df(df_combined)

# Remove NA values
df_combined_clean <- na.omit(df_combined_clean)

# Calculate correlation between 'depth' and 'sig' columns
correlation <- cor(df_combined_clean$sig, df_combined_clean$mag)
print(paste("Correlation between 'depth' and 'sig':", correlation))

ggplot(df_eq, aes(x = mag, y = sig)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Significance and magnitude", x = "Magnitude", y = "Significance") +
  theme_minimal()

# Subset the data with 'sig' and 'mag' columns
df_pca <- df_combined_clean[, c('sig', 'mag')]

# Standardize the data
scaled_data <- scale(df_pca)

# Compute PCA
pca_result <- prcomp(scaled_data, scale = TRUE)

# Add PCA scores to the original dataframe
df_combined_clean$PC1 <- pca_result$x[, 1]
df_combined_clean$PC2 <- pca_result$x[, 2]

# View the updated dataframe with PCA values
print(head(df_combined_clean))

# Plotting the data points in the new PCA space
ggplot(df_combined_clean, aes(x = PC1, y = PC2)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "PCA Plot of Earthquake Data", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

ggplot(df_combined_clean, aes(x = PC2, y = sig)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "PCA Plot of Earthquake Data", x = "PC2", y = "sig") +
  theme_minimal()




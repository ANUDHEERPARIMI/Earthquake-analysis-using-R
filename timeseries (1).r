library(data.table)
library(dplyr)
library(ggplot2)
path <- '/Users/parimianudheer/Desktop/Project 1/earthquake_time_series_LSTM/datasets/imported_usgs_api'
my_files <- list.files(path, pattern = "raw.csv", full.names = TRUE)

my_files


df_combined <- rbindlist(lapply(my_files, fread))

rownames(df_combined) <- NULLss

dim(df_combined)

head(df_combined, 5)

# Define a function to clean the data frame
clean_df <- function(df) {
  # Getting only the useful columns
  df_clean <- df[, c('type', 'time', 'coordinates', 'mag', 'place', 'status', 'tsunami', 'sig', 'net', 
                     'nst', 'dmin', 'rms', 'gap', 'magType'), drop = FALSE]
  
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

# Clean the data frame
df_combined_clean <- clean_df(df_combined)

# View the first few rows of the cleaned data frame
head(df_combined_clean)

# Read the CSV file into a DataFrame
df_eq <- read.csv("/Users/parimianudheer/Desktop/Project 1/earthquake_time_series_LSTM/datasets/combined_eq_california_clean.csv")

df_eq <- df_eq[ ,-1]

# Fix the time column datatype to datetime
df_eq$time <- as.POSIXct(df_eq$time)

# Add a name column for Folium map pop-ups
df_eq$name_mag <- paste("M:", df_eq$mag, "/", sep=" ")
df_eq$name_date <- paste(as.Date(df_eq$time), "/", sep=" ")
df_eq$name <- paste(df_eq$name_mag, df_eq$name_date, df_eq$place, sep="")
df_eq <- df_eq %>% select(-name_mag, -name_date)

# Sort the DataFrame with respect to time
df_eq <- df_eq[order(df_eq$time), ]

# Set the DataFrame index to the "time" column
df_eq <- arrange(df_eq, time)

# Impute missing depth values with the mean depth
df_eq$depth[is.na(df_eq$depth)] <- mean(df_eq$depth, na.rm = TRUE)
#dep_normalized <- (df_eq$depth - min(df_eq$depth)) / (max(df_eq$depth) - min(df_eq$depth))
#df_eq$depth <- dep_normalized
# Display the first 5 rows of the DataFrame
print(head(df_eq, 5))
summary(df_eq$mag)

# Calculate the moving average of earthquake magnitudes
window_size <- 300  # Define the window size for the moving average
df_eq$ma_mag <- zoo::rollmean(df_eq$mag, k = window_size, align = "center", fill = NA)

# Plot original magnitudes and moving average
ggplot(data = df_eq, aes(x = time)) +
  geom_point(aes(y = mag), color = "blue", alpha = 0.5) +
  geom_line(aes(y = ma_mag), color = "red") +
  labs(x = "Time", y = "Magnitude", title = "Earthquake Magnitudes and Moving Average")


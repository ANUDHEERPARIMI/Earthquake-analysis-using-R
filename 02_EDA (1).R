# Load required libraries
install.packages('httr')
library(httr)
install.packages('jsonlite')
library(jsonlite)
install.packages('tidyverse')
library(tidyverse)
install.packages('leaflet')
library(leaflet)
install.packages('geosphere')
library(geosphere)
install.packages('sf')
library(sf)
install.packages('tidyverse')
library(tidyverse)

# For geospatial plotting
install.packages('ggplot2')
library(ggplot2)

# For machine learning
install.packages('caret')
library(caret)
install.packages('cluster')
library(cluster)

# Since R does not have an exact equivalent to pickle, you might use R's built-in serialization methods like saveRDS and readRDS.
# For example:
# Saving an object:
# saveRDS(obj, "filename.rds")
# Loading the object:
# obj <- readRDS("filename.rds")


# Set the file path
file_path <- "/Users/parimianudheer/Desktop/Project 1/earthquake_time_series_LSTM/datasets/combined_eq_california_clean.csv"

# Read the CSV file into a data frame and convert it to a time-series object
df_eq <- read.csv(file_path)
df_eq$time <- as.POSIXct(df_eq$time)  # Assuming 'time' is a column containing timestamps

head(df_eq, 3)  # Display the first 3 rows of the data frame


str(df_eq)


# Set the figure format
options(repr.plot.width = 7, repr.plot.height = 4)

# Create the histogram plot
hist(log(df_eq$mag), main = "EQ magnitude", xlab = "EQ magnitude", ylab = "Frequency", col = "skyblue", border = "white")


# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Create the histogram plot for EQ depth
hist(log(df_eq$depth), main = "EQ Depth", xlab = "EQ Depth", ylab = "Frequency", col = "skyblue", border = "white")



library(ggplot2)

# Set plot options
options(repr.plot.width=7, repr.plot.height=4)

# Plot histogram
ggplot(df_eq, aes(x = longitude)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "white", log ="y") +
  labs(x = "EQ longitude", y = "Frequency", fontsize = 13) +
  theme_minimal() +
  theme(text = element_text(size = 13))



# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Create the histogram plot for EQ latitude
hist(log(df_eq$latitude), main = "EQ Latitude", xlab = "EQ Latitude", ylab = "Frequency", col = "skyblue", border = "white")


# Subset the data frame
df_eq_large <- subset(df_eq, mag > 6)

# Convert the "time" column to POSIXct format (assuming it contains timestamps)
df_eq_large$time <- as.POSIXct(df_eq_large$time)

# Calculate the time difference in days using the difftime function
time_diff <- c(NA, as.numeric(diff(df_eq_large$time), units = "days"))

# Add the time difference as a new column
df_eq_large$time_diff_day <- time_diff

# Print the head of the data frame to check the results
#head(df_eq_large)


head(df_eq_large, 5)  # Display the first 5 rows of the filtered data frame


str(df_eq_large)


nrow(df_eq_large)


summary(df_eq_large$time_diff_day)


# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Convert time_diff_day to seconds
df_eq_large$time_diff_sec <- as.numeric(df_eq_large$time_diff_day * 86400)  # 86400 seconds in a day

# Create the histogram plot for EQ time difference in seconds
hist(df_eq_large$time_diff_sec, main = "EQ Time Difference (Seconds)", xlab = "EQ Time Difference (Seconds)", ylab = "Frequency", col = "skyblue", border = "white")


# Set the figure format
options(repr.plot.width = 9, repr.plot.height = 5)

# Create a plot for the "mag" column in df_eq_large
plot(df_eq_large$mag, type = "o", col = "red", xlab = "Index", ylab = "Magnitude", main = "Magnitude")

# Add a legend
legend("topright", legend = "Magnitude", col = "red", lty = 1, cex = 1, bty = "n")


# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Create a scatter plot
plot(df_eq_large$time_diff_sec, df_eq_large$mag, 
     xlab = "EQ Time Difference (Seconds)", ylab = "Magnitude", 
     main = "Scatter Plot of EQ Time Difference vs Magnitude", 
     col = "blue", pch = 16)

# Add labels
text(df_eq_large$time_diff_sec, df_eq_large$mag, labels = df_eq_large$time_diff_day, pos = 3)


# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Create a scatter plot
plot(df_eq_large$time_diff_sec, df_eq_large$mag, 
     xlab = "EQ Time Difference (Seconds)", ylab = "Magnitude", 
     main = "Scatter Plot of EQ Time Difference vs Magnitude", 
     col = "blue", pch = 16)

# Add labels
text(df_eq_large$time_diff_sec, df_eq_large$mag, labels = df_eq_large$time_diff_day, pos = 3)


# Assuming 'df_eq_large' is a data frame with a datetime index
date <- as.Date(as.character(df_eq_large$time[1]))  # Extracting the first datetime value and converting it to date

class(date)  # Check the class of 'date'

install.packages('maps')
library(maps)
install.packages('mapdata')
library(mapdata)

# Create a new plot
plot.new()

# Define the map projection and center coordinates
map("world", col = "lightgray", xlim = c(-130, -114), ylim = c(32, 42), fill = TRUE, bg = "white")
map.axes()

# Plot EQ locations
points(df_eq_large$longitude, df_eq_large$latitude, col = "red", pch = 0.19, cex = df_eq_large$mag/5, bg = "black", lwd = 1)

# Add labels
legend("topright", legend = "Magnitude", pch = 19, col = "red", pt.cex = df_eq_large$mag/0.05, bg = "black", cex = 1, bty = "n")




install.packages('leaflet')
library(leaflet)
install.packages('htmlwidgets')
library(htmlwidgets)

install.packages("ggmap")  # Install ggmap package
library(ggmap)             # Load ggmap package



install.packages("tmaptools")  # Install tmaptools package
library(tmaptools)             # Load tmaptools package


# Install and load ggmap package
install.packages("ggmap")
library(ggmap)

# Register your Google Maps API Key
ggmap::register_google(key = "AIzaSyAYynxwGFpTlT95z2ezogWLHaXZak-IvAs")

# Geocode the location
location <- geocode("California")
print(location)



city <- "California"
location <- geocode(city)
cat(location$latitude, location$longitude)

# Assuming df_eq_large contains latitude, longitude, mag, time, and name columns
df_eq_large$color <- "black"
df_eq_large$size <- scale(df_eq_large$mag, center = FALSE, scale = c(3, 15))

# Initialize the map with the starting location
map <- leaflet() %>%
  setView(lng = location$longitude, lat = location$latitude, zoom = 7) %>%
  addTiles() %>%
  addCircleMarkers(
    data = df_eq_large,
    lng = ~longitude,
    lat = ~latitude,
    color = ~color,
    fillColor = ~color,
    fillOpacity = 0.7,
    radius = ~size,
    popup = ~name
  )

# Add legend
legend_html <- paste(
  "<div style='position:fixed; bottom:10px; left:10px; border:2px solid black; z-index:9999; font-size:14px;'>&nbsp;<b>",
  color,
  ":</b><br>&nbsp;<i class='fa fa-circle fa-1x' style='color:black'></i>&nbsp;black<br></div>",
  sep = ""
)

map <- htmlwidgets::prependContent(map, tags$div(HTML(legend_html)))

map


# Assuming df_eq has a timestamp column named 'time'
df_eq_lp <- subset(df_eq, time >= as.POSIXct("1989-09-19") & time <= as.POSIXct("1989-10-19"))

head(df_eq_lp)

install.packages("maps")
library(maps)

library(mapdata)

# Set up the plot with specified projection parameters
map("world", col = "lightgray", xlim = c(-130, -114), ylim = c(32, 42), fill = TRUE, bg = "white")
map.axes()

# Extract longitude and latitude values from df_eq_lp
lon <- df_eq_lp$longitude
lat <- df_eq_lp$latitude

# Calculate marker size based on magnitude
size <- df_eq_lp$mag / 0.05

# Plot the earthquake locations as circles
points(lon, lat, col = "red", pch = 19, cex = size, bg = "black", lwd = 1)


library(leaflet)

city <- "California"
locator <- geocode(city)
location <- c(locator$latitude, locator$longitude)

data <- df_eq_lp

# Scale magnitude values for marker sizes
data$size <- scale(data$mag, center = FALSE, scale = c(3, 15))

# Initialize the map with the starting location
map_ <- leaflet(data = data) %>%
  addTiles("CartoDB.Positron") %>%
  setView(lng = location[2], lat = location[1], zoom = 7) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~size,
    color = ~color,
    fill = TRUE,
    fillOpacity = 0.7,
    popup = ~name
  )

# Add legend
legend_html <- paste(
  "<div style='position:fixed; bottom:10px; left:10px; border:2px solid black; z-index:9999; font-size:14px;'>&nbsp;<b>",
  color,
  ":</b><br>&nbsp;<i class='fa fa-circle fa-1x' style='color:black'></i>&nbsp;black<br></div>",
  sep = ""
)

map_ <- htmlwidgets::prependContent(map_, tags$div(HTML(legend_html)))

map_


library(ggplot2)

# Assuming df_eq_lp contains the "mag" column and a datetime column
# Convert the datetime column to POSIXct format if needed
# df_eq_lp$time <- as.POSIXct(df_eq_lp$time)

# Plot the "mag" column against time
ggplot(df_eq_lp, aes(x = time, y = mag)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Magnitude", x = "Time", y = "Magnitude") +
  theme_minimal()


library(ggplot2)

# Assuming df_eq_lp contains the "depth" column and a datetime column
# Convert the datetime column to POSIXct format if needed
# df_eq_lp$time <- as.POSIXct(df_eq_lp$time)

# Plot the "depth" column against time
ggplot(df_eq_lp, aes(x = time, y = depth)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Depth", x = "Time", y = "Depth") +
  theme_minimal()


library(ggplot2)

# Assuming df_eq_lp contains the "longitude" column and a datetime column
# Convert the datetime column to POSIXct format if needed
# df_eq_lp$time <- as.POSIXct(df_eq_lp$time)

# Plot the "longitude" column against time
ggplot(df_eq_lp, aes(x = time, y = longitude)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Longitude", x = "Time", y = "Longitude") +
  theme_minimal()


library(ggplot2)

# Assuming df_eq_lp contains the "latitude" column and a datetime column
# Convert the datetime column to POSIXct format if needed
# df_eq_lp$time <- as.POSIXct(df_eq_lp$time)

# Plot the "latitude" column against time
ggplot(df_eq_lp, aes(x = time, y = latitude)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Latitude", x = "Time", y = "Latitude") +
  theme_minimal()


library(ggplot2)

# Assuming df_eq_lp contains the necessary columns: "longitude", "latitude", "mag", and an index column
# Plot the scatter plot with size and color mapped to magnitude and index respectively
ggplot(df_eq_lp, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = mag / 0.05, color = index), alpha = 0.4) +
  scale_size_continuous(range = c(3, 15)) +
  labs(title = "Earthquakes", x = "Longitude", y = "Latitude", size = "Magnitude", color = "Index") +
  theme_minimal()



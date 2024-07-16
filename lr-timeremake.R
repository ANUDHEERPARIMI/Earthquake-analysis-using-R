# Importing required libraries
library(readr)
library(dplyr)
library(Metrics)

# Reading the data
file_path <- "/Users/parimianudheer/Desktop/Project 1/earthquake_time_series_LSTM/datasets/combined_eq_california_clean.csv"
df_lp <- read_csv(file_path)

# Preparing Loma Prieta EQ's
df_lp <- df_lp %>%
  mutate(time = as.POSIXct(time),
         time_seconds = as.numeric(difftime(time, as.POSIXct("1970-01-01"))),
         time_to_failure_sec = max(time_seconds) - time_seconds) %>%
  select(-c(time, time_seconds, place, status, tsunami, net, nst, type))

# Building the model
# Step 1: Assemble X and y variables
X <- df_lp %>% select(mag, sig, depth, longitude, latitude)
y <- df_lp$time_to_failure_sec

# Step 2: Fit the model
lr_model <- lm(y ~ ., data = X)

# Step 3: Model diagnostics
summary(lr_model)

# Calculating accuracy scores
# R-squared (R2) Score
r_squared <- summary(lr_model)$r.squared
print(paste("R-squared (R2) Score:", r_squared))

# Mean Absolute Error (MAE)
mae <- mean(abs(predict(lr_model) - y))
print(paste("Mean Absolute Error (MAE):", mae))

# Root Mean Squared Error (RMSE)
mse <- mean((predict(lr_model) - y)^2)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))


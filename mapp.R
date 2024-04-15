# Load necessary packages
library(tsibble)
library(dplyr)
library(purrr)

# Create a sample tsibble (replace this with your actual data)
set.seed(123)
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2025-12-31")
dates <- seq(start_date, end_date, by = "day")
historical_data <- tibble(
  Date = dates,
  Close = rnorm(length(dates), mean = 100, sd = 10)
)

# Define a vector of starting dates you're interested in
starting_dates <- c(as.Date("2005-01-01"), as.Date("2010-01-01"), as.Date("2015-01-01"))

# Function to grab 10 rows starting from a specific date
grab_10_rows <- function(tibble_data, start_date) {
  tibble_data %>%
    filter(Date >= start_date) %>%
    select(Close) |> 
    head(10)
}

# Use purrr::map to apply the grab_10_rows function to each starting date
result_list <- map(starting_dates, ~ grab_10_rows(historical_data, .x))

glimpse(result_list)

# Combine the resulting list of data frames into a single data frame
result_df <- bind_cols(result_list)

glimpse(result_df)

# Print the result
print(result_df)

#################################
## STATS798 Thesis
## eMarket Inflow Harness
## Author: Benjamin Preston
## Date Created: 2024-08-18
## Last Modified: 2024-08-18
################################# 

# Libraries
library(tidyverse)
library(readr)

# Setup
inflows_csv_in <- "Inflows240125_HMD_IF.csv"
years <- 1931:2023 # full historic dataset
#years <- seq(1994, by = 1, length.out = 30)
inflows_csv_out = paste0("Inflows_",years[1],"_",years[length(years)],"_AVG_IF.csv")

# Load the data
# .csv inflow data from base eMarket run

inflows_wide <- read_csv(inflows_csv, col_names = FALSE) 

# Rename the columns

inflows_wide <- inflows_wide %>% 
  rename(
    `Scheme` = X1,
    `Year` = X2
  ) %>% 
  rename_with(.cols = c(3:55), 
              .fn = ~paste0("W", 1:53)
              ) %>% 
  filter(Year %in% years)

# Pivot data to long format  
inflows_long <- inflows_wide %>% 
  pivot_longer(cols = -c(Scheme, Year), 
               names_to = "Week", 
               values_to = "Inflows"
               ) %>% 
  mutate(Week = as.numeric(str_remove(Week, "W")))

# Compute weekly averages by scheme
average_row <- inflows_long %>%
  group_by(Scheme, Week) %>%
  summarise(mean = round(mean(Inflows),2)) %>%
  mutate(Year = years[1]-1) %>% 
  rename(
    `Inflows` = `mean`
  )

inflows_long_w_avg <- bind_rows(inflows_long, average_row) %>% 
  arrange(Scheme, Year, Week)


# pivot wide
# 
inflows_wide_w_avg <- inflows_long_w_avg %>% 
  pivot_wider(names_from = Week, 
              values_from = Inflows
              )

head(inflows_wide_w_avg)

# save as .csv without header rows
# 
# 

inflows_wide_w_avg %>% write_csv(inflows_csv_out, col_names = FALSE) 

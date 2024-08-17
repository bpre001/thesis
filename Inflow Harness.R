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

# Load the data
# .csv inflow data from base eMarket run

inflows_wide <- read_csv("Inflows240125_HMD_IF.csv", col_names = FALSE) 

# Rename the columns

inflows_wide <- inflows_wide %>% 
  rename(
    `Scheme` = X1,
    `Year` = X2
  ) %>% 
  rename_with(.cols = c(3:55), 
              .fn = ~paste0("W", 1:53)
              )
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
  summarise(mean(Inflows)) %>%
  mutate(Year = 1930) %>% 
  rename(
    `Inflows` = `mean(Inflows)`
  )

inflows_long_w_avg <- bind_rows(inflows_long, average_row) %>% 
  arrange(Scheme, Year, Week)

# pivot wide
# 
inflows_wide_w_avg <- inflows_long_w_avg %>% 
  pivot_wider(names_from = Week, 
              values_from = Inflows
              )

# save as .csv without header rows
# 
# 
file_name = "Inflows240125_AVG"
inflows_wide_w_avg %>% 
  write_csv(paste0(file_name,"_IF.csv"), col_names = FALSE) 

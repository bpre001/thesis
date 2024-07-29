# Load libraries

library(tidyverse)
library(fpp3)
library(viridis)

# Load Manapouri WV Table generated from full historical data inflow set
# 
Manapouri_91 <- read_csv("Apr24 Manapori WV 91.csv")

weeks = 53
burnin = 80

Manapouri_91 <- Manapouri_91 |>  
  mutate(rn = row_number(),
         weekcount = (rn-1) %% (weeks + burnin),
         other_storage = floor((rn-1)/(weeks + burnin)),
         other_storage = round(other_storage / 12 * 100),
         facet_title = paste("Other storage:", as.character(other_storage), "%")
         ) |> 
  select(-rn) |> 
  pivot_longer(-c(weekcount, other_storage, facet_title), 
               names_to = "storage", values_to = "value") |> 
  mutate(storage = round(as.double(storage)*100),
         value = as.double(value),
         value = pmax(0.25, value),
         log2value = log2(value)
         ) 

Manapouri_91|> 
  summarise(max = max(value),
            min = min(value),
            mean = mean(value),
            sd = sd(value),
            maxlog2 = max(log2value),
            minlog2 = min(log2value),
            meanlog2 = mean(log2value),
            sdlog2 = sd(log2value)
            )




Manapouri_91 |> 
  filter(other_storage == 50,
         weekcount < 53) |>
  ggplot(aes(x = weekcount, y = storage, z = log2value)) +
  geom_contour_filled(aes(fill = ..level..), breaks = seq(-4,12,by = 2)) +  # Filled contours
  geom_contour(color = "white") +  # Add contour lines for clarity
  labs(title = "Manapouri Water Values",
       fill = "log2(value)",
       x = "Week",
       y = "Manapouri Storage (%)") +
  theme_minimal() +
  facet_wrap(~facet_title, scales = "fixed")

library(tidyverse)

emily_data_raw <- read_csv("emily_baselines.csv")

emily_data_manipulated <- emily_data_raw %>% 
  gather(variable, value, - (sitecode.x:role.x)) %>%
  unite(temp, variable, role.x) %>%
  spread(temp, value)

write.csv(x = emily_data_manipulated, "emily_data_manipulated.csv")

library(tidyverse)
setwd("~/Desktop/Masters Analysis/analysis")

si <- read.csv("data/2018_isotopes.csv")

keep <- c("algae", "det", "terbase", "aqbase", "cc")

si$delta13c_norm <- si$delta13c + (-3.32 + (0.99*si$c_n_ratio))

si %>% filter(!replicate == 1) %>%
  filter(!is.na(sitecode.x)) %>%
  filter(role.x %in% keep) %>%
  group_by(sitecode.x, role.x) %>%
  summarise(d13c = mean(delta13c, na.rm = T), d13c_norm = mean(delta13c_norm, na.rm = T), 
            d15n = mean(delta15n, na.rm = T), dh2 = mean(delta2h, na.rm = T)) %>%
  ggplot(aes(x = dh2, y = d15n, color  = role.x, group = sitecode.x))+
  geom_point(size = 3)+
  facet_wrap(~sitecode.x)

baselines <- si %>% filter(!replicate == 1) %>%
  filter(!is.na(sitecode.x)) %>%
  filter(role.x %in% keep) %>%
  group_by(sitecode.x, role.x) %>%
  summarise(d13c = mean(delta13c, na.rm = T), d13c_norm = mean(delta13c_norm, na.rm = T), d15n = mean(delta15n, na.rm = T), dh2 = mean(delta2h, na.rm = T)) %>%
  filter(!role.x == "cc") %>%
  filter(!is.na(dh2)) %>% 
  gather(variable, value, - (sitecode.x:role.x)) %>%
  unite(temp, variable, role.x) %>%
  spread(temp, value)

si <- si[!si$replicate==1,]
si <- left_join(si, baselines)


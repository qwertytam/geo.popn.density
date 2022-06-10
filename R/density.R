here::i_am("R/density.R")

library(here)
library(tidyverse)

consolidated <- bind_rows(au, us)

consolidated %>% select(Country_Code, Popn_Est, Area_km2) %>%
  group_by(Country_Code) %>%
  summarise(P = sum(Popn_Est), A = sum(Area_km2)) %>%
  mutate(D = P/A)

# Calculate average weighted densities
arthimetic_d <- consolidated %>%
  group_by(Country_Code) %>%
  summarise(arthimetic_d = sum(Density * Popn_Est)/sum(Popn_Est))

geometic_d <- consolidated %>%
  group_by(Country_Code) %>%
  filter(Density != 0) %>%
  summarise(geometic_d = exp(sum(log(Density) * Popn_Est)/sum(Popn_Est)))

median_d <- consolidated %>%
  group_by(Country_Code) %>%
  summarise(median_d = median(Density))

arthimetic_d
geometic_d
median_d



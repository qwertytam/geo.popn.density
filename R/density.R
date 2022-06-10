here::i_am("R/density.R")

library(here)
library(tidyverse)
library(scales)

data(au, us)
all_countries <- bind_rows(au, us)

all_countries <- all_countries %>%
  group_by(Country_Code) %>%
  mutate(MSA_Name = if_else(is.na(MSA_Name), "Rural", MSA_Name),
         Perc_Popn = Popn_Est / sum(Popn_Est) * 100)

# Average Weighted Densities ---------------------------------------------------
avgw_densities <- all_countries %>%
  group_by(Country_Code) %>%
  filter(Density > 0) %>%
  summarise(avgw_density = sum(Density * Popn_Est)/sum(Popn_Est)) %>%
  mutate(method = "arthimetic")

avgw_densities <- all_countries %>%
  group_by(Country_Code) %>%
  filter(Density > 0) %>%
  summarise(avgw_density = exp(sum(log(Density) * Popn_Est)/sum(Popn_Est))) %>%
  mutate(method = "geometric") %>%
  bind_rows(avgw_densities)

avgw_densities <- all_countries %>%
  group_by(Country_Code) %>%
  filter(Density > 0) %>%
  summarise(avgw_density = median(Density)) %>%
  mutate(method = "median") %>%
  bind_rows(avgw_densities)

g_avgw_densities <- ggplot(avgw_densities,
                           aes(x = method,
                               y = avgw_density,
                               fill = Country_Code))
g_avgw_densities +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  geom_text(aes(label = round(avgw_density)),
            vjust = 1.6,
            color = "white",
            position = position_dodge(0.9),
            size = 3.5) +
  theme_minimal() +
  labs(x = "Weighting Method",
       y = "Density (People/km^2)",
       title = "Average Weighted Population Density", 
       fill = "Country") 

# Pareto Charts ----------------------------------------------------------------
allc_by_density <- all_countries %>%
  arrange(desc(Density)) %>%
  mutate(Cum_Perc_Popn = cumsum(Perc_Popn))

g_allc_by_density <- allc_by_density %>%
  slice_max(Density) %>%
  mutate(Cum_Perc_Popn = 0, MSA_Name = "x intercept") %>%
  bind_rows(allc_by_density) %>% 
  filter(Density > 0) %>%
  ggplot(aes(x = Cum_Perc_Popn, y = Density, color = Country_Code))
g_allc_by_density + geom_step(direction = "hv") +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme_minimal() +
  labs(x = "Cumulative Percent of Population",
       y = "Density (People/km^2)",
       title = "Statistical Area Density", 
       color = "Country") 

allc_by_density %>%
  filter(Density > 0) %>%
  mutate(Mdn_diff = Cum_Perc_Popn - 50) %>%
  filter(Mdn_diff > 0) %>%
  slice_min(Mdn_diff) %>%
  select(Country_Code, MSA_Name,SA_Code, Popn_Est,
         Area_km2, Density, Cum_Perc_Popn)

msas <- all_countries %>%
  group_by(Country_Code, MSA_Name) %>%
  mutate(MSA_Name = if_else(is.na(MSA_Name), "Rural", MSA_Name)) %>%
  summarise(Popn_Est = sum(Popn_Est),
            Area_km2 = sum(Area_km2),
            Perc_Popn = sum(Perc_Popn),
            .groups = "drop_last") %>%
  mutate(Density = Popn_Est / Area_km2)

msas_by_popn <- msas %>%
  arrange(desc(Popn_Est)) %>%
  mutate(MSA_Name = as_factor(MSA_Name)) %>%
  arrange(fct_relevel(MSA_Name, "Rural", after = Inf))  %>%
  mutate(Cum_Perc_Popn = cumsum(Perc_Popn))

g_msas_by_popn <- msas_by_popn %>%
  slice_max(Popn_Est) %>%
  mutate(Cum_Perc_Popn = 0, MSA_Name = "x intercept") %>%
  bind_rows(msas_by_popn) %>%
  mutate(Popn_Est = Popn_Est / 10^6) %>%
  ggplot(aes(x = Cum_Perc_Popn, y = Popn_Est, color = Country_Code))
g_msas_by_popn + geom_step(direction = "vh") +
  theme_minimal() +
  labs(x = "Cumulative Percent of Population",
       y = "Population (Millions)",
       title = "Metropolitan Statistical Area Population", 
       color = "Country") 

msas_by_popn %>%
  mutate(Mdn_diff = Cum_Perc_Popn - 50) %>%
  filter(Mdn_diff > 0) %>%
  slice_min(Mdn_diff) %>%
  select(Country_Code, MSA_Name, Popn_Est, Area_km2, Density, Cum_Perc_Popn)


msas_by_density <- msas %>%
  arrange(desc(Density)) %>%
  mutate(MSA_Name = as_factor(MSA_Name)) %>%
  mutate(Cum_Perc_Popn = cumsum(Perc_Popn))

g_msas_by_density <- msas_by_density %>%
  slice_max(Density) %>%
  mutate(Cum_Perc_Popn = 0, MSA_Name = "x intercept") %>%
  bind_rows(msas_by_density) %>%
  ggplot(aes(x = Cum_Perc_Popn, y = Density, color = Country_Code))
g_msas_by_density + geom_step(direction = "vh") +
  theme_minimal() +
  labs(x = "Cumulative Percent of Population",
       y = "Density (People/km^2)",
       title = "Metropolitan Statistical Area Density", 
       color = "Country") 

msas_by_density %>%
  mutate(Mdn_diff = Cum_Perc_Popn - 50) %>%
  filter(Mdn_diff > 0) %>%
  slice_min(Mdn_diff) %>%
  select(Country_Code, MSA_Name, Popn_Est, Area_km2, Density, Cum_Perc_Popn)

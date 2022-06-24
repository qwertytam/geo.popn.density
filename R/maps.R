here::here("R/maps.R")

library(sf)
library(here)
library(tidyverse)
library(geo.popn.density)

# nc_tas <- dplyr::filter(nc, State_code_2016 == 6)
# nc_tas_sa2 <- dplyr::filter(nc, SA2_maincode_2016 == 601011001)
# 
# nc_bigpopn <- dplyr::slice_max(nc, order_by = ERP_2021 ,n = 2)
# 
# inner_syd <- dplyr::filter(aus, SA3_code_2016 == 11703)
# 
# ggplot2::ggplot(dplyr::summarise(dplyr::group_by(inner_syd, SA3_code_2016))) + 
#   ggplot2::geom_sf() + 
#   ggplot2::theme_void()
# 
# colnames(au)
# colnames(aus)

fpath <- here("data/SA2 ERP GeoPackage 2021.gpkg")
aus <- st_read(fpath) %>%
  select(
    ERP_2020,
    Area_km2,
    SA2_maincode_2016
  )

head(aus)

aus_geo <- aus %>%
  left_join(
    au,
    by = c("SA2_maincode_2016" = "SA_Code")
  ) %>%
  select(
    Popn_Est,
    Area_km2.x,
    SA2_maincode_2016,
    MSA_Code,
    MSA_Name
  ) %>%
  filter(!is.na(MSA_Code)) %>%
  filter(MSA_Name != "<NA>")

head(aus_geo)

aus_geo %>%
  filter(
    MSA_Code == 1030
  ) %>%
  as_tibble() %>%
  distinct(MSA_Code) %>%
  arrange(desc(MSA_Code))

aus_msas <- aus_geo %>%
  group_by(MSA_Code) %>%
  summarise(
    popn = sum(Popn_Est),
    area = sum(Area_km2.x),
    density = popn / area,
    MSA_Code = first(MSA_Code),
    MSA_Name = first(MSA_Name)
  )

sf::sf_use_s2(FALSE)
# aus_msas <- aus_geo %>%
#   filter(MSA_Code == 1030) %>%
#   group_by(MSA_Code) %>%
#   summarise(
#     popn = sum(Popn_Est),
#     area = sum(Area_km2.x),
#     density = popn / area,
#     MSA_Code = first(MSA_Code),
#     MSA_Name = first(MSA_Name)
#   )


aus_geo$geom <- aus_geo$geom %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()

head(aus_msas)

big_msas <- aus_msas %>%
  filter(popn > 10^5)

head(big_msas)

ggplot2::ggplot(big_msas) + 
  ggplot2::geom_sf() + 
  ggplot2::theme_void()


aus_map <- summarise(aus)

ggplot2::ggplot(aus_map) + 
  ggplot2::geom_sf() + 
  ggplot2::theme_void()

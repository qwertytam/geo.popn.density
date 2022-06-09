# Script to download and wrangle the US population data.
# Writes the output to us.csv in the inst/ext directory and us.rda in the
# data directory.

here::i_am("data-raw/us.R")

library(here)
library(tidycensus)
library(tidyverse)
library(sf)
library(readxl)

ext_data_dir <- "inst/extdata"
census_api_key(Sys.getenv("US_CENSUS_DATA_API_KEY"))
options(tigris_use_cache = TRUE)

# Data gathering ---------------------------------------------------------------

# Get list of states (and equivalents) and FIPS codes
states <- get_acs(geography = "state",
                  table = "B01003",
                  cache_table = TRUE,
                  year = 2020,
                  geometry = TRUE,
                  keep_geo_vars = TRUE,
                  survey = "acs5")

state_fips <- states %>%
  as_tibble() %>%
  select(STATEFP) %>%
  unlist()

# Get list of counties (and equivalents)
counties <- get_acs(geography = "county",
                    table = "B01003",
                    cache_table = TRUE,
                    year = 2020,
                    geometry = TRUE,
                    keep_geo_vars = TRUE,
                    survey = "acs5")

# Get the population at the "block group" level
GetPopn <- function(state){
  popn <- get_decennial(geography = "block group",
                        variables = c(popn = "P1_001N"),
                        cache_table = TRUE,
                        year = 2020,
                        state = state)
  return(popn)
}

popn <- state_fips %>%
  map_dfr(GetPopn)%>%
  select(-c(NAME, variable)) %>%
  rename(Popn_Est = value)

# Get the "block group" geographies
GetGeos <- function(state){
  geo <- get_acs(geography = "block group",
                 table = "B01003",
                 cache_table = TRUE,
                 year = 2020,
                 state = state,
                 geometry = TRUE,
                 keep_geo_vars = TRUE) %>%
    filter(variable == "B01003_001")
  return(geo)
}

geos <- state_fips %>%
  map_dfr(GetGeos) %>%
  as_tibble() %>%
  select(-c(TRACTCE, BLKGRPCE, AFFGEOID, NAME.x, NAMELSAD, LSAD, NAME.y,
            variable, estimate, moe, geometry))

# Download county to metro and micro statistical area relationship mappings
fname_delin <- "list1_2020.xls"
fpath_delin <- here(ext_data_dir, fname_delin)

url_base <- "https://www2.census.gov/"
url_delin <- paste(url_base,
                   "programs-surveys/metro-micro/geographies/reference-files/",
                   "2020/delineation-files/",
                   fname_delin,
                   sep="")

res <- tryCatch(download.file(url_delin, fpath_delin), error = function(e) 1)

# Read in the excel file
first_data_row <- 3
county_msa_map <- read_excel(fpath_delin,
                             sheet = "List 1",
                             skip = first_data_row - 1) %>%
  filter(!is.na(`CBSA Code`))

# Create column to join on using county FIPS code
county_msa_map <- county_msa_map %>%
  mutate(County_FIPS = paste(`FIPS State Code`, `FIPS County Code`, sep = "")) %>%
  rename(CBSA_Code = `CBSA Code`,
         CBSA_Name = `CBSA Title`,
         Metro_Micro = `Metropolitan/Micropolitan Statistical Area`) %>%
  select(CBSA_Code,
         CBSA_Name,
         Metro_Micro,
         County_FIPS) %>% 
  mutate(Metro_Micro = recode(Metro_Micro,
                              "Metropolitan Statistical Area" = "Metro",
                              "Micropolitan Statistical Area" = "Micro",
                              .default = NA_character_,
                              .missing = NA_character_))

# Data warngling ---------------------------------------------------------------
# Combining population data with geographic, state, county and MSA data

popn_geos <- geos %>%
  left_join(popn, by = "GEOID")

popn_geos <- states %>%
  as_tibble() %>%
  select(STATEFP, STUSPS, NAME.x) %>%
  right_join(popn_geos, by = "STATEFP") %>%
  rename(State_Name = NAME.x)

popn_geos <- popn_geos %>%
mutate(County_FIPS = paste(STATEFP, COUNTYFP, sep ="")) %>%
  select(-COUNTYFP)

popn_geos <- counties %>%
  as_tibble() %>%
  select(GEOID, NAMELSAD) %>%
  rename(County_FIPS = GEOID) %>%
  right_join(popn_geos, by = "County_FIPS") %>%
  rename(County_Name = NAMELSAD)

popn_geos <- popn_geos  %>%
  left_join(county_msa_map, by = "County_FIPS") 

# Calculate total area and density in km^2
popn_geos <- popn_geos%>%
  mutate(Area_km2 = (ALAND + AWATER) / 10^6) %>%
  select(-c(ALAND, AWATER)) %>%
  filter(Area_km2 != 0 | !is.na(Area_km2)) %>%
  mutate(Density = Popn_Est / Area_km2)

popn_geos <- popn_geos %>%
  rename(State_FIPS = STATEFP,
         State_Abbr = STUSPS) %>%
  relocate(State_FIPS, State_Abbr, State_Name,
           County_FIPS, County_Name,
           CBSA_Code, CBSA_Name,
           Metro_Micro,
           GEOID) 

# Data out ---------------------------------------------------------------------

fname_out <- "us.csv"
fpath_out <- here(ext_data_dir, fname_out)
popn_geos %>%
  write_csv(fpath_out)

# Write the data out to a R data file
us <- popn_geos
usethis::use_data(us, overwrite = TRUE)

# Remove files no longer needed
if (file.exists(fpath_delin)){ file.remove(fpath_delin) }

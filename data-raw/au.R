# Script to download and wrangle the Australian population data.
# Writes the output to au.csv in the inst/ext directory and au.rda in the
# data directory.

here::i_am("data-raw/au.R")

library(here)
library(tidyverse)
library(readxl)

ext_data_dir <- "inst/extdata"

# Data gathering ---------------------------------------------------------------

# Download population  estimates and components by SA2, 2020 to 2021
fname_popn <- "32180DS0001_2020-21.xlsx"
fpath_popn <- here(ext_data_dir, fname_popn)

url_base <- "https://www.abs.gov.au/"
url_popn <- paste(url_base,
                 "statistics/people/population/regional-population/2020-21/",
                 fname_popn,
                 sep="")

res <- tryCatch(download.file(url_popn, fpath_popn), error = function(e) 1)

# Set up the column names and column types from the downloaded file
# Note skipping columns that are blank or we are ignoring
col_names = c("State_Code", "State_Name"               )
col_types = c("numeric",    "text",       "skip", "skip")

col_names = c(col_names, "County_Code", "County_Name"               )
col_types = c(col_types, "numeric",     "text",        "skip", "skip")

col_names = c(col_names, "SA_Code", "SA_Name",      "Popn_Est")
col_types = c(col_types, "numeric", "text", "skip", "numeric")
col_types = c(col_types, "skip", "skip", "skip", "skip", "skip", "skip")

col_names = c(col_names,                 "Area_km2")
col_types = c(col_types, "skip", "skip", "numeric", "skip")

# Read in the data and iterate through the relevant tabs
shts <- c("Table 1", "Table 2", "Table 3", "Table 4",
          "Table 5", "Table 6", "Table 7", "Table 8")

first_data_row <- 10
au <- map_dfr(set_names(shts, shts),
                  read_excel,
                  path = fpath_popn,
                  skip = first_data_row - 1,
                  col_names = col_names,
                  col_types = col_types)

# Clean up data frame and add relevant columns
au <- au %>%
  drop_na() %>%
  mutate(Country_Code = "AU",
         State_Abbr = case_when(
           State_Code == 1 ~ "NSW",
           State_Code == 2 ~ "VIC",
           State_Code == 3 ~ "QLD",
           State_Code == 4 ~ "SA",
           State_Code == 5 ~ "WA",
           State_Code == 6 ~ "TAS",
           State_Code == 7 ~ "NT",
           State_Code == 8 ~ "ACT",
         )) %>%
  relocate(Country_Code, .before = State_Code) %>%
  relocate(State_Abbr, .before = State_Name)

# Now get the SA2 to SUA mapping logic
fname_sa2_sua <- "1270055004_sa2_sua_2016_aust_csv.zip"
fpath_sa2_sua <- here(ext_data_dir, fname_sa2_sua)

url_sa2_sua <- paste(url_base,
                     "ausstats/subscriber.nsf/log?openagent&",
                     "1270055004_sa2_sua_2016_aust_csv.zip",
                     "&1270.0.55.004&Data%20Cubes",
                     "&D6E51168BD6DC248CA2581B1000E0A48&0",
                     "&July%202016&09.10.2017&Latest", sep="")

res <- tryCatch(download.file(url_sa2_sua, fpath_sa2_sua),
                error = function(e) 1)

# Unzip the download and remove the zip file
flist <- unzip(fpath_sa2_sua, list = TRUE)
unzip(fpath_sa2_sua, exdir = here(ext_data_dir))
if (file.exists(fpath_sa2_sua)){ file.remove(fpath_sa2_sua) }

# Update the file path to point to the first file extracted as we are
# assuming that that there is only one file in the archive
fpath_sa2_sua <- here(ext_data_dir, flist[[1]])
sa2_sua_map <- read_csv(fpath_sa2_sua,
                        col_select = c("SA2_MAINCODE_2016",
                                      "SUA_CODE_2016",
                                      "SUA_NAME_2016"),
                        col_types = "ddc")

# Data wrangling ---------------------------------------------------------------
# Join the map to the data
au <- au %>%
  left_join(sa2_sua_map, by = c("SA_Code" = "SA2_MAINCODE_2016")) %>%
  rename(MSA_Code = SUA_CODE_2016,
         MSA_Name = SUA_NAME_2016) %>%
  mutate(Metro_Micro = if_else(grepl("Not in any Significant", MSA_Name),
                               NA_character_,
                               "Metro"),
         MSA_Name = if_else(grepl("Not in any Significant", MSA_Name),
                            NA_character_,
                            MSA_Name)) %>%
  relocate(MSA_Code, MSA_Name, Metro_Micro, .before = SA_Code)

# Calculate densities
au <- au %>%
  mutate(Density = Popn_Est/Area_km2)

# Data out ---------------------------------------------------------------------
# Write the data to a csv file
fname_out <- "au.csv"
fpath_out <- here(ext_data_dir, fname_out)
au %>%
  write_csv(fpath_out)

# Write the data out to a R data file
usethis::use_data(au, overwrite = TRUE)

# Remove files no longer needed
if (file.exists(fpath_popn)){ file.remove(fpath_popn) }
if (file.exists(fpath_sa2_sua)){ file.remove(fpath_sa2_sua) }

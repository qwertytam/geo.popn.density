# Script to download and wrangle the Australian population 
# and writes the output to au.csv in the data directory.

library(here)
library(openxlsx)
library(tidyverse)
library(readxl)

# Download population data
fname <- "32180DS0001_2020-21.xlsx"

url <- "https://www.abs.gov.au/"
url <- paste(url, "statistics/people/population/regional-population/2020-21/", sep="")
url <- paste(url, fname, sep="")

data_raw_dir <- "data_raw"
fpath <- here(data_raw_dir, fname)
res <- tryCatch(download.file(url, fpath), error = function(e) 1)
if(res == 0){print("File successfully downloaded")}

# Set up the column names and column types from the downloaded file
# Note skipping columns that are blank or we are ignoring
col_names = c("State_Code", "State_Name", "GCCSA_Code", "GCCSA_Name")
col_types = c("numeric", "text", "text", "text")

col_names = c(col_names, "S4_Code", "S4_Name", "S3_Code", "S3_Name")
col_types = c(col_types, "numeric", "text", "numeric", "text")

col_names = c(col_names, "S2_Code", "S2_Name", "Popn_2021_Est")
col_types = c(col_types, "numeric", "text", "skip", "numeric")
col_types = c(col_types, "skip", "skip", "skip", "skip", "skip", "skip")

col_names = c(col_names, "Area_km2")
col_types = c(col_types, "skip", "skip", "numeric", "skip")

# Read in the data and iterate through the relevant tabs
shts <- c("Table 1", "Table 2", "Table 3", "Table 4", "Table 5")
shts <- c(shts, "Table 6", "Table 7", "Table 8")

first_data_row <- 10
tbl_df <- map_dfr(set_names(shts, shts),
               read_excel,
               path = fpath,
               skip = first_data_row - 1,
               col_names = col_names,
               col_types = col_types)

# Clean up data frame
tbl_df <- tbl_df %>% drop_na() 

# Calculate densities
tbl_df <- tbl_df %>%
  mutate(Density = Popn_2021_Est/Area_km2)

# Now get the SA2 to SUA mapping logic
fname <- "1270055004_sa2_sua_2016_aust_csv.zip"

url <- "https://www.abs.gov.au/ausstats/subscriber.nsf/"
url <- paste(url, "log?openagent&1270055004_sa2_sua_2016_aust_csv.zip", sep="")
url <- paste(url, "&1270.0.55.004&Data%20Cubes", sep="")
url <- paste(url, "&D6E51168BD6DC248CA2581B1000E0A48&0", sep="")
url <- paste(url, "&July%202016&09.10.2017&Latest", sep="")

fpath <- here(data_raw_dir, fname)
res <- tryCatch(download.file(url, fpath), error = function(e) 1)
if(res == 0){ print("File successfully downloaded") }

# Unzip the download and remove the zip file
flist <- unzip(fpath, list = TRUE)
unzip(fpath, exdir = here(data_raw_dir))
if (file.exists(fpath)){ file.remove(fpath) }

fpath <- here(data_raw_dir, flist[[1]])
sa2_sua_map <- read_csv(fpath)

# Join the map to the data
tbl_df <- tbl_df %>%
  left_join(sa2_sua_map, by = c("S2_Code" = "SA2_MAINCODE_2016"))

# Clean up the data
tbl_df <- tbl_df %>%
  select(-c(SA2_5DIGITCODE_2016, SA2_NAME_2016, AREA_ALBERS_SQKM)) %>%
  rename(SUA_Code = SUA_CODE_2016, SUA_Name = SUA_NAME_2016) %>%
  relocate(c(SUA_Code, SUA_Name), .after = S3_Name)

# Write the data to csv file
fname <- "aus.csv"
data_dir <- "data"
fpath <- here(data_dir, fname)
tbl_df %>%
  write_csv(fpath)

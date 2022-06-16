library(dplyr)
library(gt)
library(magrittr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# Set up variables
f_path <- "./R" # Relative to current working directory
f_nm_pattern <- "(\\w+)(\\.R{1})" # File name match pattern
fn_nm_pattern <- "[a-zA-z0-9.]+\\({1}" # Function name match pattern
pkgfn_nm_pattern <- "\\:{2}[a-zA-z0-9.]+" # pkg::fn name match pattern

fns_ignore <- c(
  "c",
  "function",
  "grepl",
  "numeric",
  "return"
) # Base functions to ignore in the count

# Get a list of files in the directory
f_nms <- list.files(
  path = f_path, pattern = f_nm_pattern, all.files = TRUE,
  full.names = TRUE
)

if (exists("fn_nms")) {
  remove(fn_nms)
}

# Reach each file, extract the function names, store in a tibble
count <- length(f_nms)
counti <- 0
for (file in f_nms) {
  counti <- counti + 1
  print(paste0("Starting file ", counti, " of ", count, " : ", file))
  ftext <- read_file(file = file)
  f_fn_nms <- str_extract_all(ftext, fn_nm_pattern)
  f_fn_nms <- append(f_fn_nms, str_extract_all(ftext, pkgfn_nm_pattern))
  
  f_fn_nms <- tibble(fun_names = f_fn_nms, file_name = file)
  
  if (exists("fn_nms")) {
    fn_nms <- add_row(fn_nms, f_fn_nms)
  } else {
    fn_nms <- f_fn_nms
  }
}

fn_nms <- fn_nms %>%
  mutate(file_name = str_replace_all(
    file_name,
    pattern = paste0(f_path, "/"), replacement = ""
  ))

fn_nms <- fn_nms %>%
  unnest(cols = c(fun_names)) %>%
  mutate(
    fun_names = str_replace_all(
      fun_names,
      pattern = "\\(", replacement = ""
    ),
    fun_names = str_replace_all(
      fun_names,
      pattern = "\\:{2}", replacement = ""
    )
  )

funique <- fn_nms %>%
  count(fun_names) %>%
  filter(!fun_names %in% fns_ignore) %>%
  nest_join(fn_nms)

funique <- funique %>%
  unnest(cols = "fn_nms") %>%
  distinct() %>%
  nest(cols = "file_name")
tab_all <- funique %>%
  arrange(desc(n)) %>%
  gt() %>%
  tab_header("All Functions Occuring in this Package") %>%
  cols_label(
    fun_names = "Function Names",
    n = "Count",
    cols = "File Names"
  )
tab_all
tab_all %>%
  gtsave("tab_all_fns.html", path = paste0(getwd(), "/html/"))
writeLines(paste0("Table saved to dir: ", getwd(), "/html/"))

cutoff <- 6

funique %>%
  filter(n >= cutoff) %>%
  arrange(desc(n)) %>%
  gt() %>%
  tab_header(paste0(
    "Functions Occuring At Least ",
    cutoff,
    " Times in this Package"
  )) %>%
  cols_label(
    fun_names = "Function Names",
    n = "Count",
    cols = "File Names"
  )

funique %>%
  filter(n < cutoff) %>%
  arrange(desc(n)) %>%
  gt() %>%
  tab_header(paste0(
    "Functions Occuring No More Than ",
    cutoff,
    " Times in this Package"
  )) %>%
  cols_label(
    fun_names = "Function Names",
    n = "Count",
    cols = "File Names"
  )

rm(list = ls())
detach(package:dplyr)
detach(package:gt)
detach(package:magrittr)
detach(package:readr)
detach(package:stringr)
detach(package:tibble)
detach(package:tidyr)
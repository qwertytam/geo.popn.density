# Data Gathering ---------------------------------------------------------------

#' Combines and returns all country data
#'
#' `Popn_Est` is altered so that all statistical areas are included in all
#' density calculations e.g. the `geometric` (see `?CalcDensities`) calculation
#' cannot deal with a density of 0
#'
#' @return A tibble with the same columns of the package country data except
#' as follows:
#' \describe{
#'   \item{Area_km2}{Only areas with > 0 are kept, all others are excluded}
#'   \item{MSA_Name}{`NA` entries in this column are replaced with `Rural`}
#'   \item{Popn_Est}{All areas with a population of 0 are changed to a population of 1}
#'   \item{Perc_Popn}{New column with percentage of each countries' population}
#'   \item{Density}{Updated based on new `Popn_Est` figure}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
GetAllData <- function(){
  data <- dplyr::bind_rows(au, us) %>%
    dplyr::filter(.data$Area_km2 > 0) %>%
    dplyr::group_by(.data$Country_Code) %>%
    dplyr::mutate(
      MSA_Name = dplyr::if_else(is.na(.data$MSA_Name), "Rural", .data$MSA_Name),
      Popn_Est = dplyr::if_else(.data$Popn_Est == 0, 1, .data$Popn_Est),
      Perc_Popn = .data$Popn_Est / sum(.data$Popn_Est) * 100,
      Density = .data$Popn_Est / .data$Area_km2,
    ) %>%
    dplyr::ungroup()
  
  return(data)
}


# General Data Sorting & Calculation Functions ---------------------------------


#' Group the statistical area data by desired grouping
#'
#' @param data A tibble of country data e.g. from `geo.popn.density::GetAllData`
#' Data required to have the following columns:
#' \describe{
#'   \item{ppn_est}{Population estimate}
#'   \item{area}{Geographical area}
#'   \item{ppn_pct}{Percent of population that each row represents}
#'   \item{density}{Population density}
#' }
#' @param data A tibble of country data e.g. from \code{geo.popn.density::GetAllData}
#' @param ppn_est Column name that contains the population estimate; default is "ppn_est"
#' @param area Column name that contains the area; default is "area"
#' @param ppn_pct Column name that contains the population percentage for each area; default is "ppn_pct"
#' @param density Column name that contains the density; default is "density"
#' @param group_by A character vector containing the column names in `data`
#'   to group and calculate the cumulative percentages for e.g.
#'   `c('Country_Code', 'State_Name)`. If `NULL` then no grouping will occur.
#' @return A tibble with the same columns as the all country data
#' @importFrom magrittr %>%
#' @importFrom rlang := !!! sym syms
#' @export
GroupData <- function(data,
                      ppn_est = "ppn_est",
                      area = "area",
                      ppn_pct = "ppn_pct",
                      density = "density",
                      group_by = NULL){
  
  datag <- data
  
  if(!purrr::is_null(group_by)){
    datag <- datag %>%
      dplyr::group_by(!!! syms(group_by))
  }  
  
  datag <- datag %>%
    dplyr::summarise("{ppn_est}" := sum(eval(sym(ppn_est))),
                     "{area}" := sum(eval(sym(area))),
                     "{ppn_pct}" := sum(eval(sym(ppn_pct))),
                     .groups = "drop_last") %>%
    dplyr::mutate("{density}" := eval(sym(ppn_est)) / eval(sym(area)))
  
  return(datag)
}


#' Sort data by descending metric and add cumulative percentage column
#'
#' If specified, will calculate cumulative percentage for each group e.g. by country
#'
#' Mainly used to calculate the cumulative percentage population for data
#' sorted by population density.
#'
#' @param data A data frame or equivalent (e.g. tibble) to sort
#' @param cum_pct The name for the new; default is "Cum_Perc"
#' @param metric The column in `data` that contains the metric (e.g. population)
#'   to calculate the cumulative percentage of. Required, will exit if `NULL`.
#' @param sort_by The column in `data` to sort in descending order e.g. density.
#'   Required, will exit if if `NULL`.
#' @param group_by A character vector containing the column names in `data`
#'   to group and calculate the cumulative percentages for e.g.
#'   `c('Country_Code', 'State_Name)`. If specified will remove existing grouping.
#'   If `NULL` then no grouping will occur.
#' @return A data frame of the same type as passed with data sorted in
#'   descending order by `sort_by` and grouped if specified with the additional
#'   column
#'   \describe{
#'     \item{cum_pct}{Cumulative percent of the given metric}
#'   }
#' @importFrom magrittr %>% 
#' @importFrom rlang !!! := sym syms
#' @export
CalcCumPercent <- function(data,
                           cum_pct = "Cum_Perc",
                           metric = NULL,
                           sort_by = NULL,
                           group_by = NULL){
  
  if(purrr::is_null(metric)) stop("`metric` cannot be `NULL`")
  
  if(purrr::is_null(sort_by)) stop("`sort_by` cannot be `NULL`")
  
  if(!purrr::is_null(group_by)){
    data <- data %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!! syms(group_by))
  }
  
  data <- data %>%
    dplyr::arrange(dplyr::desc(eval(sym(sort_by)))) %>%
    dplyr::mutate(
      "{cum_pct}" :=
        cumsum(eval(sym(metric))) / sum(eval(sym(metric))) * 100
    )
  
  return(data)
}


#' Find the median row by specified metric
#'
#' Mainly used to find the median population density for each country. Note,
#' `base::median()` finds the median value by sorting the data frame and
#' returning the median (i.e. middle) row. This function is intended to be used
#' to find the row where the cumulative percentage (typically of population)
#' is >= 50%.
#'
#' @param data A data frame or equivalent (e.g. tibble) to find the median row
#' @param cum_pct The column in `data` that contains the cumulative
#'   percentage to sort by. If `NULL` then will create and add the column to
#'   to `data` with the default name of `Cum_Perc`.
#' @return A data frame of the same type as passed with the median row for each
#'   group if `data` is grouped
#' @importFrom magrittr %>%
#' @importFrom rlang .data sym
#' @export
FindMedian <- function(data, cum_pct = NULL){
  
  if(purrr::is_null(cum_pct)) stop("`cum_pct` cannot be `NULL`")
  
  mdn <- data %>%
    dplyr::mutate(Mdn_diff = eval(sym(cum_pct)) - 50) %>%
    dplyr::filter(.data$Mdn_diff > 0) %>%
    dplyr::slice_min(.data$Mdn_diff) %>%
    dplyr::select(!(.data$Mdn_diff))
  
  return(mdn)
}


#' Calculates population densities for each country using different methods
#' 
#' Calculates using the following methods:
#' \describe{
#'   \item{simple arithmetic}{\eqn{\frac{\sum{population}}{\sum{Area}}}}
#'   \item{weighted average arithmetic}{\eqn{\frac{\sum(density * population)}{\sum{population}}}}
#'   \item{weighted average geometric}{\eqn{\exp(\frac{\sum(\log(density) * population)}{\sum{population}})}}
#'   \item{median}{\eqn{median(density)}}
#' }
#'
#' @param data A tibble of country data e.g. from \code{geo.popn.density::GetAllData}
#' @param ccode Column name that contains the country code; default is "country_code"
#' @param ppn_est Column name that contains the population estimate; default is "ppn_est"
#' @param area Column name that contains the area; default is "area"
#' @param density Column name that contains the density; default is "density"
#' @param group_by A character vector containing the column names in `data`
#'   to group and calculate the densities for e.g.
#'   `c('Country_Code', 'State_Name)`. If specified will remove existing
#'   grouping. If `NULL` then no grouping will occur.
#' @return A tibble with columns as follows:
#' \describe{
#'   \item{`ccode`}{Upper case ISO 3166 code; column name will be the same as given by `ccode`}
#'   \item{`density`}{Population density per area unit; column name will be the same as given by `density`}
#'   \item{method}{Character description of calculation method}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang !!! := syms sym
#' @export
#'
CalcDensities <- function(data,
                          ccode = "country_code",
                          ppn_est = "ppn_est",
                          area = "area",
                          density = "density",
                          group_by = NULL){
  
  if(!purrr::is_null(group_by)){
    data <- data %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!! syms(group_by))
  }  
  
  
  # Simple mean
  densities <- data %>%
    dplyr::group_by(!!! syms(ccode)) %>%
    dplyr::summarise(
      "{density}" := sum(eval(sym(ppn_est))) / sum(eval(sym(area)))) %>%
    dplyr::mutate(method = "simple")
  
  # Arthimetic weighted average
  densities <- data %>%
    dplyr::group_by(!!! syms(ccode)) %>%
    dplyr::summarise(
      "{density}" :=
        sum(eval(sym(density)) * eval(sym(ppn_est))) / sum(eval(sym(ppn_est)))) %>%
    dplyr::mutate(method = "arthimetic") %>%
    dplyr::bind_rows(densities)
  
  # Geometric weighted average
  densities <- data %>%
    dplyr::group_by(!!! syms(ccode)) %>%
    dplyr::summarise(
      "{density}" := exp(
        sum(log(eval(sym(density))) * eval(sym(ppn_est))) / sum(eval(sym(ppn_est))))
    ) %>%
    dplyr::mutate(method = "geometric") %>%
    dplyr::bind_rows(densities)
  
  # Median by cumulative population
  data_m <- CalcCumPercent(data,
                           cum_pct = "cum_popn_pct",
                           metric = ppn_est,
                           sort_by = density,
                           group_by = ccode)

  data_m <- FindMedian(data_m, cum_pct = "cum_popn_pct")

  densities <- data_m %>%
    dplyr::select(!!! syms(c(ccode, density))) %>%
    dplyr::mutate(method = "median") %>%
    dplyr::bind_rows(densities) %>%
    dplyr::ungroup()
  
  return(densities)
}


# Data Sorting & Calculations at the Metropolitan Area Level -------------------


#' Sort metropolitan area data by descending population
#'
#' @param msas A tibble of MSAs data e.g. from `geo.popn.density::GroupbyMSA`
#' @param cum_pct The name for the new cumulative percentage column; default is `cum_pct`
#' @param ppn_est Column name that contains the population estimate; default is "ppn_est"
#' @param ppn_pct Column name that contains the population percentage for each area; default is "ppn_pct"
#' @param msa_name Column name that contains the MSA name; default is "msa_name"
#' @param group_by A character vector containing the column names in `data`
#'   to group and sort by. Specifying this will remove all other grouping.
#'   If `NULL` then no grouping will occur.
#' @return A tibble with the same columns as the msa data except as follows:
#' \describe{
#'   \item{Rural}{Entries in the `MSA_Name` column placed last in the tibble}
#'   \item{Cum_Perc_Popn}{Cumulative sum of population percentage}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang := sym
#' @export
SortMSAsbyPopn <- function(msas,
                           cum_pct = "cum_pct",
                           ppn_est = "ppn_est",
                           ppn_pct = "ppn_pct",
                           msa_name = "msa_name",
                           group_by = NULL){
  
  if(!purrr::is_null(group_by)){
    msas_by_popn <- dplyr::ungroup(msas)
    msas_by_popn <- dplyr::group_by(msas_by_popn, !!! syms(group_by))
  }  
  
  msas_by_popn <- msas_by_popn %>%
    dplyr::arrange(dplyr::desc(eval(sym(ppn_est)))) %>%
    dplyr::mutate("{msa_name}" :=
                    forcats::as_factor(eval(sym(msa_name)))) %>%
    dplyr::mutate("{msa_name}" := 
                    forcats::fct_relevel(eval(sym(msa_name)),
                                         "Rural",
                                         after = Inf))  %>%
    dplyr::mutate("{cum_pct}" := cumsum(eval(sym(ppn_pct))))
  
  return(msas_by_popn)
}


#' Sort metropolitan area data by descending population density
#'
#' @param msas A tibble of msa data e.g. from `geo.popn.density::GroupbyMSA`
#' @param cum_pct  Column name that contains the cumulative percentage; default is "cum_pct"
#' @param density Column name that contains the density estimate; default is "density"
#' @param ppn_pct Column name that contains the population percentage for each area; default is "ppn_pct"
#' @param group_by A character vector containing the column names in `data`
#'   to group and sort by. Specifying this will remove all other grouping.
#'   If `NULL` then no grouping will occur.
#' @return A tibble with the same columns as the msa data except as follows:
#' \describe{
#'   \item{Cum_Perc_Popn}{Cumulative sum of population percentage}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data syms sym
#' @export
SortMSAsbyDensity <- function(msas,
                              cum_pct = "cum_pct",
                              density = "density",
                              ppn_pct = "ppn_pct",
                              group_by = NULL){
  
  if(!purrr::is_null(group_by)){
    msas_by_density <- dplyr::ungroup(msas)
    msas_by_density <- dplyr::group_by(msas_by_density, !!! syms(group_by))
  }  
  
  msas_by_density <- msas_by_density %>%
    dplyr::arrange(dplyr::desc(eval(sym(density)))) %>%
    dplyr::mutate("{cum_pct}" := cumsum(eval(sym(ppn_pct))))
  
  return(msas_by_density)
}

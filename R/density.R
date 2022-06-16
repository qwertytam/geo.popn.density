# Data Gathering ---------------------------------------------------------------

#' Combines and returns all country data
#'
#' @return A tibble with the same columns of the country data except as follows:
#' \describe{
#'   \item{MSA_Name}{`NA` entries in this column are replaced with `Rural`}
#'   \item{Perc_Popn}{New column with percentage of each countries' population}
#' }
#' @importFrom dplyr bind_rows group_by mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
GetAllData <- function(){
  utils::data(au, us)
  all_countries <- bind_rows(au, us) %>%
    group_by(.data$Country_Code) %>%
    mutate(
      MSA_Name = dplyr::if_else(is.na(.data$MSA_Name), "Rural", .data$MSA_Name),
      Perc_Popn = .data$Popn_Est / sum(.data$Popn_Est) * 100
    )
  
  return(all_countries)
}


# Data Sorting & Calculations a the Statistical Area Level ----------------------

#' Calculates average weighted densities for each country
#' 
#' Calculates using the arithmetic, geometric, and median methods
#'
#' @param all_countries A tibble of country data e.g. from
#'   \code{geo.popn.density::GetAllData}
#' @return A tibble columns as follows:
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{avgw_density}{Average weighted population density in km^2}
#'   \item{method}{Calculation density}
#' }
#' @importFrom dplyr bind_rows filter group_by mutate summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
CalcAvgWDensities <- function(all_countries){
  avgw_densities <- all_countries %>%
    group_by(.data$Country_Code) %>%
    filter(.data$Density > 0) %>%
    summarise(
      avgw_density = sum(.data$Density * .data$Popn_Est)/sum(.data$Popn_Est)) %>%
    mutate(method = "arthimetic")
  
  avgw_densities <- all_countries %>%
    group_by(.data$Country_Code) %>%
    filter(.data$Density > 0) %>%
    summarise(
      avgw_density = exp(sum(log(.data$Density) * .data$Popn_Est)/sum(.data$Popn_Est))) %>%
    mutate(method = "geometric") %>%
    bind_rows(avgw_densities)
  
  avgw_densities <- all_countries %>%
    group_by(.data$Country_Code) %>%
    filter(.data$Density > 0) %>%
    summarise(
      avgw_density = stats::median(.data$Density)) %>%
    mutate(method = "median") %>%
    bind_rows(avgw_densities)
  
  return(avgw_densities)
}


#' Sort data by descending density
#'
#' @param all_countries A tibble of country data e.g. from
#'   \code{geo.popn.density::GetAllData}
#' @return A tibble with the same columns of the country data sorted in
#'   descending order by population density by country and with the additional
#'   column
#'   \describe{
#'   \item{Cum_Perc_Popn}{Cumulative percent of the population}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
AllDataDscDensity <- function(all_countries){
  allc_by_density <- all_countries %>%
    dplyr::arrange(dplyr::desc(.data$Density)) %>%
    dplyr::mutate(Cum_Perc_Popn = cumsum(.data$Perc_Popn))

  return(allc_by_density)
}


#' Find the median statistical area by population density
#'
#' @param allc_by_density A tibble of country data e.g. from
#'   \code{geo.popn.density::AllDataDscDensity}
#' @return A tibble with columns as follows:
#'   \describe{
#'     \item{Country_Code}{Cumulative percent of the population}
#'     \item{MSA_Name}{Metropolitan area name}
#'     \item{SA_Code}{Statistical area code}
#'     \item{Popn_Est}{Population estimate}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
FindMedianSAbyDensity <- function(allc_by_density){
  mdn_sa <- allc_by_density %>%
    filter(.data$Density > 0) %>%
    mutate(Mdn_diff = .data$Cum_Perc_Popn - 50) %>%
    filter(.data$Mdn_diff > 0) %>%
    dplyr::slice_min(.data$Mdn_diff) %>%
    dplyr::select(.data$Country_Code,
                  .data$MSA_Name,
                  .data$SA_Code,
                  .data$Popn_Est,
                  .data$Area_km2,
                  .data$Density,
                  .data$Cum_Perc_Popn)

  return(mdn_sa)
}


# Data Sorting & Calculations at the Metropolitan Area Level -------------------

#' Group the statistical area level by metropolitan area
#'
#' @param all_countries A tibble of country data e.g. from
#'   \code{geo.popn.density::GetAllData}
#' @return A tibble with the same columns as the all country data
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
GroupbyMSA <- function(all_countries){
  msas <- all_countries %>%
    dplyr::group_by(.data$Country_Code, .data$MSA_Name) %>%
    dplyr::mutate(
      MSA_Name = dplyr::if_else(is.na(.data$MSA_Name),
                                "Rural",
                                .data$MSA_Name)) %>%
    summarise(Popn_Est = sum(.data$Popn_Est),
              Area_km2 = sum(.data$Area_km2),
              Perc_Popn = sum(.data$Perc_Popn),
              .groups = "drop_last") %>%
    mutate(Density = .data$Popn_Est / .data$Area_km2)

  return(msas)
}


#' Sort metropolitan area data by population descending
#'
#' @param msas A tibble of msa data e.g. from
#'   \code{geo.popn.density::GroupbyMSA}
#' @return A tibble with the same columns as the msa data except as follows:
#'   \code{Rural} entries in the \code{MSA_Name} column placed last
#'   \code{Cum_Perc_Popn} cumulative sum of population percentage
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
ArrangeMSAsbyPopn <- function(msas){
  msas_by_popn <- msas %>%
    dplyr::arrange(dplyr::desc(.data$Popn_Est)) %>%
    mutate(MSA_Name = forcats::as_factor(.data$MSA_Name)) %>%
    dplyr::arrange(forcats::fct_relevel(.data$MSA_Name,
                                        "Rural",
                                        after = Inf))  %>%
    mutate(Cum_Perc_Popn = cumsum(.data$Perc_Popn))
  
  return(msas_by_popn)
}


#' Sort metropolitan area data by descending population desnity
#'
#' @param msas A tibble of msa data e.g. from
#'   \code{geo.popn.density::GroupbyMSA}
#' @return A tibble with the same columns as the msa data except as follows:
#'   \code{Cum_Perc_Popn} cumulative sum of population percentage
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
ArrangeMSAsbyDensity <- function(msas){
  msas_by_density <- msas %>%
    dplyr::arrange(dplyr::desc(.data$Density)) %>%
    mutate(MSA_Name = forcats::as_factor(.data$MSA_Name)) %>%
    mutate(Cum_Perc_Popn = cumsum(.data$Perc_Popn))
  
  return(msas_by_density)
}


#' Find the median metropolitan area by population density
#'
#' @param msas_by_popn A tibble of msa data e.g. from
#'   \code{geo.popn.density::ArrangeMSAsbyPopn}
#' @return A tibble with columns as follows:
#'   \describe{
#'     \item{Country_Code}{Cumulative percent of the population}
#'     \item{MSA_Name}{Metropolitan area name}
#'     \item{Popn_Est}{Population estimate}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
FindMedianMSAbyPopulation <- function(msas_by_popn){
  mdn_msa <- msas_by_popn %>%
    filter(.data$Density > 0) %>%
    mutate(Mdn_diff = .data$Cum_Perc_Popn - 50) %>%
    filter(.data$Mdn_diff > 0) %>%
    dplyr::slice_min(.data$Mdn_diff) %>%
    dplyr::select(.data$Country_Code,
                  .data$MSA_Name,
                  .data$Popn_Est,
                  .data$Area_km2,
                  .data$Density,
                  .data$Cum_Perc_Popn)
  
  return(mdn_msa)
}


#' Find the median metropolitan area occurence by population density
#'
#' @param msas_by_density A tibble of msa data e.g. from
#'   \code{geo.popn.density::ArrangeMSAsbyDensity}
#' @return A tibble with columns as follows:
#'   \describe{
#'     \item{Country_Code}{Cumulative percent of the population}
#'     \item{MSA_Name}{Metropolitan area name}
#'     \item{Popn_Est}{Population estimate}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
FindMedianMSAOccurencebyDensity <- function(msas_by_density){
  mdn_msa <- msas_by_density %>%
    mutate(Mdn_diff = .data$Cum_Perc_Popn - 50) %>%
    filter(.data$Mdn_diff > 0) %>%
    dplyr::slice_min(.data$Mdn_diff) %>%
    dplyr::select(.data$Country_Code,
                  .data$MSA_Name,
                  .data$Popn_Est,
                  .data$Area_km2,
                  .data$Density,
                  .data$Cum_Perc_Popn)
  
  return(mdn_msa)
}

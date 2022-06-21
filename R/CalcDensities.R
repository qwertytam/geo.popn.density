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

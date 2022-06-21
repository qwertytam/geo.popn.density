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

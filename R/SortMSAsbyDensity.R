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

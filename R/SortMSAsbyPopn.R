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

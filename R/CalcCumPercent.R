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

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

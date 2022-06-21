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
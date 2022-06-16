#' Plot average weighted densities for each country by method
#'
#' @param avgw_densities A tibble of country data e.g. from
#'   \code{geo.popn.density::CalcAvgWDensities}
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{avgw_density}{Average weighted population density in km^2}
#'   \item{method}{Calculation density}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
PlotAvgWDensities <- function(avgw_densities){
  g_avgw_densities <- ggplot2::ggplot(avgw_densities,
                                      ggplot2::aes(x = .data$method,
                                                   y = .data$avgw_density,
                                                   fill = .data$Country_Code))
  g_avgw_densities +
    ggplot2::geom_bar(stat = "identity",
                      position = ggplot2::position_dodge()) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$avgw_density)),
                       vjust = 1.6,
                       color = "white",
                       position = ggplot2::position_dodge(0.9),
                       size = 3.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Weighting Method",
                  y = "Density (People/km^2)",
                  title = "Average Weighted Population Density",
                  fill = "Country")
}


#' Plot pareto chart of density vs. cumulative population
#'
#' @param allc_by_density A tibble of country data e.g. from
#'   \code{geo.popn.density::AllDataDscDensity}
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
PlotAllDataByDensity <- function(allc_by_density){
  g_allc_by_density <- allc_by_density %>%
    dplyr::slice_max(.data$Density) %>%
    dplyr::mutate(Cum_Perc_Popn = 0, MSA_Name = "x intercept") %>%
    dplyr::bind_rows(allc_by_density) %>%
    dplyr::filter(.data$Density > 0) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$Cum_Perc_Popn,
                   y = .data$Density,
                   color = .data$Country_Code))
  g_allc_by_density +
    ggplot2::geom_step(direction = "hv") +
    ggplot2::scale_y_continuous(
      trans = scales::log10_trans(),
      breaks = scales::breaks_log(n = 5, base = 10),
      labels = scales::label_log(base = 10, digits = 2)
      ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Cumulative Percent of Population",
         y = "Density (People/km^2)",
         title = "Statistical Area Density",
         color = "Country")
}


# g_msas_by_popn <- msas_by_popn %>%
#   slice_max(Popn_Est) %>%
#   mutate(Cum_Perc_Popn = 0, MSA_Name = "x intercept") %>%
#   bind_rows(msas_by_popn) %>%
#   mutate(Popn_Est = Popn_Est / 10^6) %>%
#   ggplot(aes(x = .data$Cum_Perc_Popn, y = Popn_Est, color = .data$Country_Code))
# g_msas_by_popn + geom_step(direction = "vh") +
#   theme_minimal() +
#   labs(x = "Cumulative Percent of Population",
#        y = "Population (Millions)",
#        title = "Metropolitan Statistical Area Population", 
#        color = "Country") 
# 
# g_msas_by_density <- msas_by_density %>%
#   slice_max(.data$Density) %>%
#   mutate(Cum_Perc_Popn = 0, MSA_Name = "x intercept") %>%
#   bind_rows(msas_by_density) %>%
#   ggplot(aes(x = .data$Cum_Perc_Popn, y = .data$Density, color = .data$Country_Code))
# g_msas_by_density + geom_step(direction = "vh") +
#   theme_minimal() +
#   labs(x = "Cumulative Percent of Population",
#        y = "Density (People/km^2)",
#        title = "Metropolitan Statistical Area Density", 
#        color = "Country") 
# 
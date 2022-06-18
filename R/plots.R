#' Plot densities for each country by method
#'
#' @param densities A tibble of country data e.g. from `geo.popn.density::CalcDensities`
#' @param density Column name that contains the density; default is "density"
#' @param ccode Column name that contains the country code; default is "country_code"
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{Density}{Population density in km^2}
#'   \item{Method}{Calculation method}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang sym .data
#' @export
#'
PlotDensities <- function(densities,
                          density = "density",
                          ccode = "country_code"){
  interval <- 500
  ylim_max <- densities %>%
    dplyr::ungroup() %>%
    dplyr::summarise(Lim = ceiling(max(eval(sym(density))) / interval) * interval) %>%
    dplyr::pull()

  g_avgw_densities <- ggplot2::ggplot(densities,
                                      ggplot2::aes(x = .data$method,
                                                   y = eval(sym(density)),
                                                   fill = eval(sym(ccode))))
  g_avgw_densities +
    ggplot2::geom_bar(stat = "identity",
                      position = ggplot2::position_dodge()) +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(ggplot2::aes(label = round(eval(sym(density)))),
                       vjust = -1.6,
                       color = "black",
                       position = ggplot2::position_dodge(0.9),
                       size = 3.5) +
    ggplot2::labs(x = "Weighting Method",
                  y = bquote(Density (People/km^2)),
                  title = "Population Density",
                  fill = "Country") +
    ggplot2::ylim(0, ylim_max)
}


#' Plot pareto chart of density vs. cumulative population
#'
#' @param allc_by_density A tibble of country data e.g. from `geo.popn.density::AllDataDscDensity`
#' @param density Column name that contains the density; default is "density"
#' @param ccode Column name that contains the country code; default is "country_code"
#' @param cum_pct  Column name that contains the cumulative percentage; default is "cum_pct"
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @export
PlotAllDataByDensity <- function(allc_by_density,
                                 density = "density",
                                 ccode = "country_code",
                                 cum_pct = "cum_pct"){
  g_allc_by_density <- allc_by_density %>%
    dplyr::slice_max(eval(sym(density))) %>%
    dplyr::mutate(Cum_Popn_Perc = 0, MSA_Name = "x intercept") %>%
    dplyr::bind_rows(allc_by_density) %>%
    dplyr::filter(eval(sym(density)) > 0) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = eval(sym(cum_pct)),
                   y = eval(sym(density)),
                   color = eval(sym(ccode))))
  g_allc_by_density +
    ggplot2::geom_step(direction = "hv") +
    ggplot2::scale_y_continuous(
      trans = scales::log10_trans(),
      breaks = scales::breaks_log(n = 5, base = 10),
      labels = scales::label_log(base = 10, digits = 2)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Cumulative Percent of Population",
                  y = bquote(Density (People/km^2)),
                  title = "Statistical Area Density",
                  color = "Country")
}


#' Plot average weighted densities for each country by method
#'
#' @param msas_by_popn A tibble of country data e.g. from `geo.popn.density::CalcAvgWDensities`
#' @param ppn_est Column name that contains the population estimate; default is "ppn_est"
#' @param ccode Column name that contains the country code; default is "country_code"
#' @param cum_pct  Column name that contains the cumulative percentage; default is "cum_pct"
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{Density}{Average weighted population density in km^2}
#'   \item{Method}{Calculation density}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @export
#'
PlotMSAsByPopn <- function(msas_by_popn,
                           ppn_est = "ppn_est",
                           ccode = "country_code",
                           cum_pct = "cum_pct"){
  g_msas_by_popn <- msas_by_popn %>%
    dplyr::slice_max(eval(sym(ppn_est))) %>%
    dplyr::mutate(Cum_Popn_Perc = 0, MSA_Name = "x intercept") %>%
    dplyr::bind_rows(msas_by_popn) %>%
    dplyr::mutate(Popn_Est = eval(sym(ppn_est)) / 10^6) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = eval(sym(cum_pct)),
                   y = eval(sym(ppn_est)),
                   color = eval(sym(ccode)))
    )
  
  g_msas_by_popn +
    ggplot2::geom_step(direction = "vh") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Cumulative Percent of Population",
                  y = "Population (Millions)",
                  title = "Metropolitan Statistical Area Population",
                  color = "Country")
}


#' Plot average weighted densities for each country by method
#'
#' @param msas_by_density A tibble of country data e.g. from `geo.popn.density::CalcAvgWDensities`
#' @param density Column name that contains the density; default is "density"
#' @param ccode Column name that contains the country code; default is "country_code"
#' @param cum_pct  Column name that contains the cumulative percentage; default is "cum_pct"
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{avgw_density}{Average weighted population density in km^2}
#'   \item{method}{Calculation density}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang sym .data
#' @export
#'
PlotMSAsByDensity <- function(msas_by_density,
                              density = "density",
                              ccode = "country_code",
                              cum_pct = "cum_pct"){
  g_msas_by_density <- msas_by_density %>%
    dplyr::slice_max(eval(sym(density))) %>%
    dplyr::mutate(Cum_Popn_Perc = 0, MSA_Name = "x intercept") %>%
    dplyr::bind_rows(msas_by_density) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = eval(sym(cum_pct)),
                   y = eval(sym(density)),
                   color = eval(sym(ccode)))
    )
  
  g_msas_by_density +
    ggplot2::geom_step(direction = "vh") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Cumulative Percent of Population",
                  y = bquote(Density (People/km^2)),
                  title = "Metropolitan Statistical Area Density",
                  color = "Country")
}
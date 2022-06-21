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
#' @importFrom rlang sym
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
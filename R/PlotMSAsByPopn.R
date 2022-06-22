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
PlotMSAsByPopn <- function(
    msas_by_popn,
    ppn_est = "ppn_est",
    ccode = "country_code",
    cum_pct = "cum_pct"
){
  
  msas_by_popn %>%
    dplyr::slice_max(eval(sym(ppn_est))) %>%
    dplyr::mutate(Cum_Popn_Perc = 0, MSA_Name = "x intercept") %>%
    dplyr::bind_rows(msas_by_popn) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = eval(sym(cum_pct)),
        y = eval(sym(ppn_est)),
        color = eval(sym(ccode))
      )
    )  +
    ggplot2::geom_step(direction = "vh") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Cumulative Percent of Population",
      y = "Population (Millions)",
      title = "Metropolitan Statistical Area Population",
      color = "Country"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_comma(suffix = "%")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(scale = 10^-6)
    )
}



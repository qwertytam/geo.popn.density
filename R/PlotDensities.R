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
PlotDensities <- function(
    densities,
    density = "density",
    ccode = "country_code"
){
  
  interval <- 500
  
  ylim_max <- densities %>%
    dplyr::ungroup() %>%
    dplyr::summarise(
      Lim = ceiling(
        max(eval(sym(density))) / interval) * interval
    ) %>%
    dplyr::pull()
  
  densities %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = tools::toTitleCase(.data$method),
        y = eval(sym(density)),
        fill = eval(sym(ccode))
      )
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = ggplot2::position_dodge()
    ) +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = format(
          round(eval(sym(density))),
          big.mark = ","
        )
      ),
      vjust = -1.6,
      color = "black",
      position = ggplot2::position_dodge(0.9),
      size = 3.5
    ) +
    ggplot2::labs(
      x = "Weighting Method",
      y = expression(paste("Density (People/km"^"2)")),
      title = "Population Density",
      fill = "Country"
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(vjust = -1.5),
      axis.title.y = ggplot2::element_text(vjust = 2),
      plot.title = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(10, 0, 10, 0),
        size = 14
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(
        big.mark = ",",
        decimal.mark = "."
      ),
      limits = c(0, ylim_max)
    )
}

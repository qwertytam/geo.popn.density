#' Plot pareto chart of density vs. cumulative population
#'
#' @param allc_by_density A tibble of country data e.g. from `geo.popn.density::AllDataDscDensity`
#' @param density Column name that contains the density; default is "density"
#' @param ccode Column name that contains the country code; default is "country_code"
#' @param cum_pct  Column name that contains the cumulative percentage; default is "cum_pct"
#' @param mdn_sa  Tibble containing median entries e.g. from `geo.popn.density::FindMedian`
#' @importFrom magrittr %>%
#' @importFrom rlang sym .data
#' @export
PlotAllDataByDensity <- function(
    allc_by_density,
    density = "density",
    ccode = "country_code",
    cum_pct = "cum_pct",
    mdn_sa
){
  
  allc_by_density <- allc_by_density %>%
    dplyr::slice_max(eval(sym(density))) %>%
    dplyr::mutate(Cum_Popn_Perc = 0, MSA_Name = "x intercept") %>%
    dplyr::bind_rows(allc_by_density) %>%
    dplyr::filter(eval(sym(density)) > 0)
  
  mdn_sa %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Cum_Popn_Perc,
        y = .data$Density,
        color = .data$Country_Code
      )
    ) + 
    ggplot2::geom_step(
      data = allc_by_density,
      ggplot2::aes(
        x = .data$Cum_Popn_Perc,
        y = .data$Density,
        color = .data$Country_Code
      ),
      direction = "hv"
    ) +
    ggplot2::geom_point() +
    ggrepel::geom_label_repel(
      ggplot2::aes(
        label = format(round(.data$Density), big.mark = ","),
        fontface = "bold"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Cumulative Percent of Population",
      y = bquote(Density (People/km^2)),
      title = "Statistical Area Density",
      color = "Country"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(
        big.mark = ",",
        decimal.mark = "."
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_comma(suffix = "%")
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 10^4))
  
}

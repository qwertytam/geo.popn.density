#' Show table with median result for each group
#'
#' @param data A tibble of median data e.g. from `geo.popn.density::FindMedian`
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{Popn_Est}{Population estimate}
#'   \item{Area_km2}{Area in km^2}
#' }
#' @return A reactable formatted table
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom reactable colDef colFormat
#' @importFrom htmltools div img span
#' @export
#'
TableCountryTotals <- function(data){
  imgpath <- "https://raw.githubusercontent.com/qwertytam/geo.popn.density/master/notebook/images/"
  
  tbl <- data %>%
    dplyr::group_by(.data$Country_Code) %>%
    dplyr::summarise(
      Popn_Est = sum(.data$Popn_Est),
      Area_km2 = sum(.data$Area_km2),
      Density = .data$Popn_Est / .data$Area_km2) %>%
    reactable::reactable(
      defaultColDef = colDef(
        align = "center",
        minWidth = 80,
      ),
      columns = list(
        Country_Code = colDef(
          name = "Country",
          defaultSortOrder = "asc",
          cell = function(value) {
            div(
              class = "country",
              img(class = "country-flag",
                  alt = paste(value, "flag"),
                  src = paste(imgpath, value, ".png", sep = "")),
              div(
                span(class = "country-name", value),
              )
            )
          }
        ),
        Popn_Est = colDef(
          name = "Population Estimate",
          minWidth = 100,
          format = colFormat(digits = 0,
                             separators = TRUE,
                             locales = "en-GB")
        ),
        Area_km2 = colDef(
          name = "Area (km<sup>2</sup>)",
          html = TRUE,
          format = colFormat(digits = 0,
                             separators = TRUE,
                             locales = "en-GB")
        ),
        Density = colDef(
          name = "Density (Population/km<sup>2</sup>)",
          minWidth = 100,
          html = TRUE,
          format = colFormat(digits = 2,
                             separators = TRUE,
                             locales = "en-GB")
        )
      ),
      borderless = TRUE,
      highlight = TRUE,
      class = "dflt-table"
    )
  
  return(tbl)
}

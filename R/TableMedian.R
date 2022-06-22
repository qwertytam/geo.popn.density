#' Show table with median result for each group
#'
#' @param data A tibble of median data e.g. from `geo.popn.density::FindMedian`
#' @param show_cols List that contains the columns to display from
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{MSA_Name}{Metropolitan satistical area name}
#'   \item{SA_Code}{Statistical area code}
#'   \item{Popn_Est}{Population estimate}
#'   \item{Area_km2}{Area in km^2}
#'   \item{Density}{Population density in km^2}
#'   \item{Cum_Popn_Perc}{Cumulative population percent}
#' }
#' @return A reactable formatted table
#' @importFrom magrittr %>%
#' @importFrom rlang syms
#' @importFrom reactable colDef colFormat
#' @importFrom htmltools div img span
#' @export
#'
TableMedian <- function(data, show_cols){
  imgpath <- "https://raw.githubusercontent.com/qwertytam/geo.popn.density/master/notebook/images/"
  
  tbl <- data %>%
    dplyr::select(!!! syms(show_cols)) %>%
    reactable::reactable(
      defaultColDef = colDef(
        header = function(value) gsub("_", " ", value, fixed = TRUE),
        align = "center",
        minWidth = 80,
      ),
      columns = list(
        Country_Code = colDef(
          show = "Country_Code" %in% colnames(.),
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
        MSA_Name = colDef(
          show = "MSA_Name" %in% colnames(.),
          minWidth = 100
        ),
        SA_Code = colDef(
          show = "SA_Code" %in% colnames(.),
          cell = function(value) as.character(value),
          minWidth = 110
        ),
        Popn_Est = colDef(
          show = "Popn_Est" %in% colnames(.),
          name = "Population Estimate",
          minWidth = 100,
          format = colFormat(digits = 0,
                          separators = TRUE,
                          locales = "en-GB")
        ),
        Area_km2 = colDef(
          show = "Area_km2" %in% colnames(.),
          name = "Area (km<sup>2</sup>)",
          html = TRUE,
          format = colFormat(digits = 2,
                          separators = TRUE,
                          locales = "en-GB")
        ),
        Density = colDef(
          show = "Density" %in% colnames(.),
          name = "Density (Population/km<sup>2</sup>)",
          minWidth = 100,
          html = TRUE,
          format = colFormat(digits = 2,
                          separators = TRUE,
                          locales = "en-GB")
        ),
        Cum_Popn_Perc = colDef(
          show = "Cum_Popn_Perc" %in% colnames(.),
          name = "Cum. Population %",
          minWidth = 100,
          format = colFormat(suffix = "%", digits = 2)
        )
      ),
      borderless = TRUE,
      highlight = TRUE,
      class = "dflt-table"
    )
  
  return(tbl)
}

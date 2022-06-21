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
ShowMedianTable <- function(data, show_cols){
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
                  src = sprintf("images/%s.png", value)),
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
ShowCountryTotals <- function(data){
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
                  src = sprintf("images/%s.png", value)),
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


#' Show table of summary statistics for each group by country
#'
#' @param data A tibble of median data e.g. from `geo.popn.density::FindMedian`
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{Popn_Est}{Population estimate}
#'   \item{Area_km2}{Area in km^2}
#' }
#' @param group_by A character vector containing the column names in `data`
#'   to group and calculate the cumulative percentages for e.g.
#'   `c("State_Name")`. If `NULL` then only grouping by `Country_Code` will occur.
#' @param stat_col Column name to calculate the stats on e.g. `area`
#' @return A reactable formatted table
#' @importFrom magrittr %>%
#' @importFrom rlang .data syms
#' @importFrom reactable colDef colFormat
#' @importFrom htmltools div img span
#' @importFrom tidyselect vars_select_helpers
#' @export
#'
ShowStats <- function(data, group_by = NULL, stat_col){
  
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$Country_Code, !!! syms(c(group_by, stat_col)))

  if(!purrr::is_null(group_by)){
    data <- data %>%
      dplyr::group_by(.data$Country_Code, !!! syms(group_by)) %>%
      dplyr::summarise("{stat_col}" := sum(eval(sym(stat_col))))
  }
  
  tbl <- data %>%
    dplyr::group_by(.data$Country_Code) %>%
    dplyr::summarise(
      Count = dplyr::n(),
      Min = min(!!! syms(stat_col)),
      Median = median(!!! syms(stat_col)),
      Mean = mean(!!! syms(stat_col)),
      Max = max(!!! syms(stat_col)),
      "Std. Dev." = sd(!!! syms(stat_col))
    ) %>%
    reactable::reactable(
      defaultColDef = colDef(
        align = "center",
        minWidth = 80,
        format = colFormat(digits = 2,
                           separators = TRUE,
                           locales = "en-GB")
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
                  src = sprintf("images/%s.png", value)),
              div(
                span(class = "country-name", value),
              )
            )
          }
        ),
        Count = colDef(
          format = colFormat(digits = 0,
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
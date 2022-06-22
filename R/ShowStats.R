#' Show table of summary statistics for each group by country
#'
#' @param data A tibble of median data e.g. from `geo.popn.density::FindMedian`
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{Popn_Est}{Population estimate}
#'   \item{Area_km2}{Area in km^2}
#'   \item{Optional: Additional group columns}{e.g. MSA_Name, State_Name}
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
  imgpath <- "https://raw.githubusercontent.com/qwertytam/geo.popn.density/master/notebook/images/"
  
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
      Median = stats::median(!!! syms(stat_col)),
      Mean = mean(!!! syms(stat_col)),
      Max = max(!!! syms(stat_col)),
      "Std. Dev." = stats::sd(!!! syms(stat_col))
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
                  src = paste(imgpath, value, ".png", sep = "")),
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
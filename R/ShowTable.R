#' Show table in html div with css class
#'
#' @param tbl Table to show
#' @param css_class String name of css class
#' @export
#'
ShowTable <- function(tbl, css_class){
  htmltools::div(class = css_class, tbl)
}
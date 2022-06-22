#' Initialize css stylesheet reference
#'
#' @export
InitCSS <- function(){
  htmltools::tags$link(
    href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback",
    rel = "stylesheet")
}
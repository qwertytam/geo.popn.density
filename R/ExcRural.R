#' Sort data by descending metric and add cumulative percentage column
#'
#' If specified, will calculate cumulative percentage for each group e.g. by country
#'
#' Mainly used to calculate the cumulative percentage population for data
#' sorted by population density.
#'
#' @param data A data frame or equivalent (e.g. tibble) to sort
#' @param msa_name Column name for the metroplitan area names
#' @param msa_code Column name for the metroplitan area codes
#' @return A data frame of the same type as passed
#' @importFrom magrittr %>% 
#' @importFrom rlang !!! := sym syms
#' @export
ExcRural <- function(data, msa_name, msa_code){
  data <- data %>%
    dplyr::filter(
      eval(sym(msa_name)) != "Rural",
      !is.na(eval(sym(msa_name))),
      !is.na(eval(sym(msa_code))),
    )
  
  return(data)
}

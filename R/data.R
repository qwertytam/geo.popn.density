#' Australian population and population densities at the SA2 level.
#' Each row is a population estimate, area and population density for a SA2.
#'
#' This dataset was produced from Australian Bureau of Statistics data
#'  available from  \url{https://www.abs.gov.au/}.
#'
#' @format A data frame with 2288 rows and 15 variables:
#' \describe{
#'   \item{State_Code}{State code}
#'   \item{State_Name}{State name}
#'   \item{GCCSA_Code}{Greater Capital City Statistical Area code}
#'   \item{GCCSA_Name}{GCCSA name}
#'   \item{S4_Code}{Statistical Area level 4 code}
#'   \item{S4_Name}{Statistical Area level 4 name}
#'   \item{S3_Code}{Statistical Area level 3 code}
#'   \item{S3_Name}{Statistical Area level 3 name}
#'   \item{SUA_Code}{Significant Urban Area code}
#'   \item{SUA_Name}{SUA name}
#'   \item{S2_Code}{Statistical Area level 2 code}
#'   \item{S2_Name}{Statistical Area level 2 name}
#'   \item{Popn_Est}{Population estimate, currenlty for 2021}
#'   \item{Area_km2}{Area of the relevant SA2}
#'   \item{Density}{Derived density}
#' }
#'
"au"
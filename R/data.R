#' Data for Australian
#' Population and population densities at the ABS SA2 level
#'
#' This dataset was produced from Australian Bureau of Statistics data
#'  available from  \url{https://www.abs.gov.au/}. See the references section
#'  in the readme for further detail on Australian data and definitions.
#'
#' @format A data frame with 2288 rows and 14 variables:
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{State_Code}{ABS state code}
#'   \item{State_Abbr}{2 or 3 letter state abbreviation}
#'   \item{State_Name}{State name}
#'   \item{County_Code}{County code - `NA` for AU data}
#'   \item{County_Name}{County name - `NA` for AU data}
#'   \item{MSA_Code}{ABS significant urban area code}
#'   \item{MSA_Name}{ABS significant urban area name}
#'   \item{Metro_Micro}{`Metro` if a metropolitan area, `NA` otherwise}
#'   \item{SA_Code}{ABS statistical area level 2 code}
#'   \item{SA_Name}{ABS statistical area level 2 name}
#'   \item{Popn_Est}{Population estimate, currenlty for 2021}
#'   \item{Area_km2}{Area of the relevant statistical area}
#'   \item{Density}{Derived density}
#' }
#'
"au"

#' Data for the United States
#' 
#' US population and population densities at the census 'tract' level.
#' 
#' This dataset was produced from US Census Bureau data
#'  available from  \url{https://www.census.gov/"/}.
#'
#' @format A data frame with 85062 rows and 14 variables:
#' \describe{
#'   \item{Country_Code}{Upper case ISO 3166 code}
#'   \item{State_Code}{State FIPS code}
#'   \item{State_Abbr}{Two letter USPS state abbreviation}
#'   \item{State_Name}{State name}
#'   \item{County_Code}{County or equivalent FIPS code}
#'   \item{County_Name}{County or equivalent name}
#'   \item{MSA_Code}{Core based statistical area code}
#'   \item{MSA_Name}{Core based statistical area name}
#'   \item{Metro_Micro}{If the tract is in a metro or micropolitan area}
#'   \item{SA_Code}{GEOID of tract}
#'   \item{SA_Name}{Name of the tract}
#'   \item{Popn_Est}{Population estimate, currenlty from the 2020 census}
#'   \item{Area_km2}{Area of the relevant tract}
#'   \item{Density}{Derived density}
#' }
#'
"us"
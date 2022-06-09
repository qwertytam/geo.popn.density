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

#' US population and population densities at the census 'block group' level.
#' Each row is a population estimate, area and population density for a census
#' block group.
#'
#' This dataset was produced from US Census Bureau data
#'  available from  \url{https://www.census.gov/"/}.
#'
#' @format A data frame with 241898 rows and 12 variables:
#' \describe{
#'   \item{State_FIPS}{State code}
#'   \item{State_Abbr}{Two letter USPS state abbreviation}
#'   \item{State_Name}{State name}
#'   \item{County_FIPS}{County or equivalent FIPS code}
#'   \item{County_Name}{County or equivalent name}
#'   \item{CBSA_Code}{Core Based Statistical Area code}
#'   \item{CBSA_Node}{Core Based Statistical Area name}
#'   \item{Metro_Micro}{If the block group is in a metro or micropolitan area}
#'   \item{S3_Name}{Statistical Area level 3 name}
#'   \item{GEOID}{GEOID of block group?}
#'   \item{Popn_Est}{Population estimate, currenlty from the 2020 census}
#'   \item{Area_km2}{Area of the relevant block group}
#'   \item{Density}{Derived density}
#' }
#'
"us"
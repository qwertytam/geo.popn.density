% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{us}
\alias{us}
\title{Data for the United States}
\format{
A data frame with 85062 rows and 14 variables:
\describe{
\item{Country_Code}{Upper case ISO 3166 code}
\item{State_Code}{State FIPS code}
\item{State_Abbr}{Two letter USPS state abbreviation}
\item{State_Name}{State name}
\item{County_Code}{County or equivalent FIPS code}
\item{County_Name}{County or equivalent name}
\item{MSA_Code}{Core based statistical area code}
\item{MSA_Name}{Core based statistical area name}
\item{Metro_Micro}{If the tract is in a metro or micropolitan area}
\item{SA_Code}{GEOID of tract}
\item{SA_Name}{Name of the tract}
\item{Popn_Est}{Population estimate, currenlty from the 2020 census}
\item{Area_km2}{Area of the relevant tract}
\item{Density}{Derived density}
}
}
\usage{
us
}
\description{
US population and population densities at the census 'tract' level.
}
\details{
This dataset was produced from US Census Bureau data
available from  \url{https://www.census.gov/"/}.
}
\keyword{datasets}

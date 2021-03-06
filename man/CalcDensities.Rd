% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcDensities.R
\name{CalcDensities}
\alias{CalcDensities}
\title{Calculates population densities for each country using different methods}
\usage{
CalcDensities(
  data,
  ccode = "country_code",
  ppn_est = "ppn_est",
  area = "area",
  density = "density",
  group_by = NULL
)
}
\arguments{
\item{data}{A tibble of country data e.g. from \code{geo.popn.density::GetAllData}}

\item{ccode}{Column name that contains the country code; default is "country_code"}

\item{ppn_est}{Column name that contains the population estimate; default is "ppn_est"}

\item{area}{Column name that contains the area; default is "area"}

\item{density}{Column name that contains the density; default is "density"}

\item{group_by}{A character vector containing the column names in \code{data}
to group and calculate the densities for e.g.
\verb{c('Country_Code', 'State_Name)}. If specified will remove existing
grouping. If \code{NULL} then no grouping will occur.}
}
\value{
A tibble with columns as follows:
\describe{
\item{\code{ccode}}{Upper case ISO 3166 code; column name will be the same as given by \code{ccode}}
\item{\code{density}}{Population density per area unit; column name will be the same as given by \code{density}}
\item{method}{Character description of calculation method}
}
}
\description{
Calculates using the following methods:
\describe{
\item{simple arithmetic}{\eqn{\frac{\sum{population}}{\sum{Area}}}}
\item{weighted average arithmetic}{\eqn{\frac{\sum(density * population)}{\sum{population}}}}
\item{weighted average geometric}{\eqn{\exp(\frac{\sum(\log(density) * population)}{\sum{population}})}}
\item{median}{\eqn{median(density)}}
}
}

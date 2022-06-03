
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geo_popn_density

<!-- badges: start -->
<!-- badges: end -->

The goal of geo_popn_density is to find the population densities for
selected countries.

## Installation

You can install the development version of geo_popn_density like so:

``` r
This is a simple script, so just download from GitHub...hopefully!
```

## Australian Data

Data sourced from the Australian Bureau of Statistics. Population data
sourced at the Statistical Area 2 level. See references below for
further detail on Australian statistical geographic standards. Density
is given by the ABS but is recalculated here as the
population-area-density numbers do not line up as given. Data is output
to `au.csv` in the data directory.

### References:

-   (<https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026#asgs-diagram>)\[Description
    of the Australian Statistical Geography Standard\]
-   (<https://www.abs.gov.au/statistics/people/population/regional-population/2020-21#data-download>)\[ABS
    population data by SA2\]
-   (<https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.004July%202016?OpenDocument>)\[Significant
    Urban Area to SA2\]

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(geo_popn_density)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

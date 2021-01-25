
<!-- README.md is generated from README.Rmd. Please edit that file -->

# snowdl

<!-- badges: start -->

[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/badge/repo%20status-WIP-yellow.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/bsmity13/snowdl)
<!-- badges: end -->

The goal of `snowdl` is to make it easy to download snow data (*e.g.*,
snow depth, snow density, snow-water equivalent) from various sources
(*e.g.*, SNODAS, Daymet).

## Installation

Install the development version from
[GitHub](https://github.com/bsmity13/snowdl) with:

``` r
# install.packages("devtools")
devtools::install_github("bsmity13/snowdl")
```

This package is not yet operational. Check back soon for progress\!

## Available data sources

*Sorted alphabetically by dataset name*

| Dataset          | Source | Link                                                                                                                                                      | Citation                                                                                                                                                 |
| :--------------- | :----- | :-------------------------------------------------------------------------------------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Daymet v4        | NASA   | <a href = 'https://daac.ornl.gov/DAYMET/guides/Daymet_Daily_V4.html' target='_blank' rel='noopener noreferrer'>ORNL DAAC</a>                              | <a href = 'https://daymet.ornl.gov/files/Thornton_Daymet_V4_submitted_2021-01-20.pdf' target='_blank' rel='noopener noreferrer'>Thornton et al. 2020</a> |
| ERA5-Land Hourly | ECMWF  | <a href = 'https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.e2161bac?tab=overview' target='_blank' rel='noopener noreferrer'>ERA5-Land</a> | DOI: <a href = 'https://doi.org/10.24381/cds.e2161bac' target='_blank' rel='noopener noreferrer'>10.24381/cds.e2161bac</a>                               |
| SNODAS           | NOAA   | <a href = 'https://nsidc.org/data/g02158' target='_blank' rel='noopener noreferrer'>NSIDC</a>                                                             | DOI: <a href = 'https://doi.org/10.7265/N5TB14TC' target='_blank' rel='noopener noreferrer'>10.7265/N5TB14TC</a>                                         |

## All text below here default

<br/> <br/> <br/> <br/>

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(snowdl)
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
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!

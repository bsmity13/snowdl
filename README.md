
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
(see below).

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

## Important Notes

  - Daymet is probably not a great source for SWE. Based on my reading,
    they treat it more like a nuisance parameter than a quanity of
    interest. See this link for their [Snow Water Equivalent
    Clarification](https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html#qualityassess).
    Note, this clarification was made in reference to V3, but still
    applies based on newest literature for V4.

  - Further, Daymet has (apparently) different units for SWE than other
    sources (but not really). Daymet uses kg/m^2, while other sources
    use m (depth if snow was melted). To convert, multiply by area of
    the pixel (kg/m^2 \* m^2/1 = kg) to get mass of water, convert to
    cubic meters (1 kg = 1 L H2O = 0.001 m^3), and then get depth by
    dividing by area (m^3/1 \* 1/m^2 = m). Perhaps more useful to have
    units in mm, so again multiply by 0.001 m = 1 mm.  
    \* Note Daymet comes with lcc projection and units = m. Resolution
    is 1000 x 1000, so area is 1e6 m^2.  
    \* Therefore, conversion becomes `x * 1e6 * 0.001 * 0.001`. All
    conversions cancel and resulting units are mm.  
    \* To be clear, kg/m^2 = mm in this case.

## Package Status/To-dos

  - [x] Basic functionality
      - [x] Download Daymet and process
      - [x] Download ERA5 and process
      - [x] Download SNODAS and process
  - [ ] User-friendly wrappers
      - [ ] Iterate over dates
      - [ ] Complete entire workflow in `tempdir()`
  - [ ] Other data sources?
  - [ ] Vignettes
      - [ ] Overview/Quickstart using wrapper functions
      - [ ] In-depth using basic functionality

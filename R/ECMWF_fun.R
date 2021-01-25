# Functions to download data from ECMWF
# (European Centre for Medium-range Weather Forecasts)

# Users need to be registered to use these functions

#' Set ECMWFR key
#'
#' Wrapper function to set user's ECMWFR key
#'
#' @param ... All arguments passed to `ecmwfr::wf_set_key()`
#'
#' @seealso \code{\link[ecmwfr:wf_set_key]{wf_set_key}()}
#'
#' @examples
#'
#' \dontrun{
#'
#' e_key(user = "test@mail.com", key = "123", service = "cds")
#'
#' }
#'
#' @export
e_key <- function(...){
  ecmwfr::wf_set_key(...)
}

#' Create ECMWFR Request
#'
#' Creates a list with data request parameters
#'
#' @param variable `[character]` Only one of `"snow_density"`,
#' `"snow_depth"`, `"snow_depth_water_equivalent"`.
#' @param years `[character]` Years for which to download data.
#' @param months `[character]` Months for which to download data.
#' @param days `[character]` Days for which to download data.
#' @param time `[character = "12:00"]` Hours for which to download data,
#' formatted as HH:MM. Hourly data are available for this dataset.
#' @param area `[numeric]` Vector specifying bounding box of spatial
#' subset to download. Should be in lat/lon, counter-clockwise from top.
#' Order is c(ymax, xmin, ymin, xmax). Default downloads all of USA.
#' @param out_file `[character]` Name of output file. Should end in ".nc".
#'
#' @details See vignette "CDS functionality" in package `ecmwfr` for
#' details on creating requests. Requests can also be created by visiting
#' web API, downloading python code, and using an RStudio function to
#' convert to list.
#'
#' @seealso
#' \href{https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html}{CDS functionality vignette}
#'
#' @return List
#'
#' @examples
#'
#' req_list <- e_request(variable = "snow_density",
#'                       years = 2018:2019,
#'                       months = 3:4,
#'                       days = 15,
#'                       out_file = "test.nc")
#'
#' @export
e_request <- function(variable = c("snow_density",
                                   "snow_depth",
                                   "snow_depth_water_equivalent"),
                      years,
                      months,
                      days,
                      time = "12:00",
                      area = c(49, -125, 24.5, -66.5),
                      out_file) {

  # Convert y/m/d/t to character
  years <- as.character(years)
  months <- as.character(months)
  days <- as.character(days)
  time = as.character(time)

  # Construct request list
  request <- list(
    variable = variable,
    year = years,
    month = months,
    day = days,
    time = time,
    area = area,
    format = "netcdf",
    dataset_short_name = "reanalysis-era5-land",
    target = out_file
  )
  # Return
  return(request)
}

#' Get ECMWF Data
#'
#' Download ECMWF data from a request
#'
#' @param ... All arguments passed to `ecmwfr::wf_request()`
#'
#' @seealso \code{\link[ecmwfr:wf_request]{wf_request}()}
#'
#' @return Returns character string with path to downloaded file.
#'
#' @examples
#'
#' \dontrun{
#'
#' e_key(user = "112233", key = "123456789", service = "cds")
#'
#' req_list <- e_request(variable = "snow_density",
#'                       years = 2018:2019,
#'                       months = 3:4,
#'                       days = 15,
#'                       out_file = "test.nc")
#'
#' get_request(user = "112233",
#'             request = req_list,
#'             transfer = TRUE,
#'             path = ".",
#'             verbose = TRUE)
#'
#' }
#'
#' @export
get_request <- function(...) {
  ecmwfr::wf_request(...)
}

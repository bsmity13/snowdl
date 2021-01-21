# Functions to download data from ECMWF
# (European Centre for Medium-range Weather Forecasts)

# Users need to be registered to use these functions

#' Set ECMWFR key
#'
#' Wrapper function to set user's ECMWFR key
#'
#' @params ... All arguments passed to ecmwfr::wf_set_key
#'
#' @seealso \code{\link[ecmwfr::wf_set_key]{wf_set_key}}()
#'
#' @examples
#'
#' \dontrun{
#'
#' set_ecmwfr_key(user = "test@mail.com", key = "123", service = "cds")
#'
#' }
#'
#' @export
set_ecmwfr_key <- function(...){
  ecmwfr::wf_set_key(...)
}

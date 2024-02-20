# All sources structure their data differently. Some store chunks of dates
# together in a single file, and some store the dates separately.
#
# `snowdl` needs a consistent format for dates that can be accessed by each
# set of functions to efficiently download and handle the range of dates a
# user may request.
#
# Here, I am using S3 classes and data.frames to store the desired dates.

# My idea is to use a data.frame where each row is a desired date. This can
# be expanded when users download and/or unpack data, including the location
# of the relevant files and a record of success/failure.

# Helper functions for snowdl_dates objects ----
# Set class
class_snowdl_dates <- function(x) {
  class(x) <- c("snowdl_dates", class(x))
  return(x)
}

# Check class
is.snowdl_dates <- function(x) {
  return(is(x, "snowdl_dates"))
}

# Plot
plot.snowdl_dates <- function(x, y, ...) {
  plot(x = x$doy, y = x$year,
       pch = 16,
       xlab = "Day-of-Year",
       ylab = "Year", ...)
}

# Coerce a vector ----
#' Create `snowdl_dates` from a vector
#'
#' Creates an object of class `snowdl_dates` from a vector of desired dates
#'
#' @param x `[Date]` The vector of dates for which to download data.
#'
#' @details This function assumes you want every date from start to end.
#'
#' @examples
#'
#' \dontrun{
#'
#' my_seq <- seq(as.Date("2020-01-01"), as.Date("2020-01-19"), by = "3 days")
#' as_snowdl_dates(my_seq)
#'
#' }
#'
#' @export
as_snowdl_dates <- function(x) {
  # Check that 'x' is a 'Date' object
  if (!(is(x, "Date"))) {
    stop("'x' must be a vector of 'Date' objects")
  }

  # Create data.frame
  df <- data.frame(date = x)
  # Add year and day-of-year
  df$year <- yr(df$date)
  df$doy <- doy(df$date)

  # Sort by date
  df <- df[order(df$date), ]

  # Set class
  df <- class_snowdl_dates(df)

  # Return
  return(df)
}

# Min-max function ----
#' Create sequence of `snowdl_dates`
#'
#' Creates an object of class `snowdl_dates` from a start and end date
#'
#' @param start `[Date]` The first date for which to download data.
#' @param end `[Date]` The last date for which to download data.
#'
#' @details This function assumes you want every date from start to end.
#'
#' @examples
#'
#' \dontrun{
#'
#' snowdate_seq(as.Date("2020-01-01"), as.Date("2020-01-15"))
#'
#' }
#'
#' @export
snowdate_seq <- function(start, end) {
  # Check that 'start' and 'end' are 'Date' objects
  if (!(is(start, "Date") & is(end, "Date"))) {
    stop("Both 'start' and 'end' should be formatted as 'Date' objects")
  }

  # Create seq
  seq <- seq.Date(from = start, to = end, by = "1 day")

  # Create data.frame
  df <- as_snowdl_dates(seq)

  # Return
  return(df)
}

# Year and DOY function ----
#' Create `snowdl_dates` for combinations of year and DOY
#'
#' Creates an object of class `snowdl_dates` from combinations of year and
#' day-of-year
#'
#' @param y `[numeric]` A vector of years for which to download data.
#' @param doy `[numeric]` A vector of day-of-year (1:366) values for which to
#' download data.
#' @param all_combos `[logical = TRUE]` Should `snowdl` create all combinations
#' of `y` and `doy` or just match (or recycle) each value. See details.
#'
#' @details This function makes it easier to get a subset of days across
#' various years.
#'
#' The argument `all_combos` decides whether you are asking for
#' all combinations of `y` and `doy` (`TRUE`) or if you want to match each `y`
#' with a single `doy` (using R's recycling rules if necessary). I.e.,
#' `all_combos = TRUE` uses `expand.grid()` whereas `all_combos = FALSE` uses
#' `data.frame()` directly.
#'
#' @examples
#'
#' \dontrun{
#'
#' # All combinations of year and day-of-year
#' snowdate_ydoy(y = 2020:2021, doy = 1:6, all_combos = TRUE)
#'
#' # Match vectors year and day-of-year (with recycling)
#' snowdate_ydoy(y = 2020:2021, doy = 1:6, all_combos = FALSE)
#'
#' }
#'
#' @export
snowdate_ydoy <- function(y, doy, all_combos = TRUE) {
  # Handle combos
  if (all_combos) {
    df <- expand.grid(date = NA, year = y, doy = doy)
  } else {
    df <- data.frame(date = NA, year = y, doy = doy)
  }

  # Assign date
  df$date <- strptime(paste(df$y, df$doy, sep = "-"), format = "%Y-%j")

  # Sort
  df <- df[order(df$date), ]
  row.names(df) <- NULL

  # Set class
  df <- class_snowdl_dates(df)

  # Return
  return(df)

}

# Functions for downloading Daymet snow data

# See example here: https://daymet.ornl.gov/files/daymet_tile-nc-retrieval.txt

# Note, for YNP, we want tiles 12095, 12096

# This should download the SWE data instead of all data

#' Download Daymet SWE Data
#'
#' Downloads snow-water equivalent data from Daymet
#'
#' @param year `[numeric]` Year(s) for which to download data.
#' @param tile `[numeric]` Tile(s) for which to download data. See details.
#' @param out_dir `[character = "data"]` The directory where data will be
#' downloaded. It will be created if it does not exist.
#'
#' @details This function creates a text string for the correct URL for each
#' year/tile combination, which will be passed to `utils::download.file()` to
#' download the data.
#'
#' More details on arguments:
#'   *  `year` can be a vector of any number of years. It will be combined with
#'   `tile` to return all combinations of `year` and `tile`.
#'   *  `tile` can be a vector of any number of tiles. A figure with the Daymet
#'   tiles can be found
#'   [here](https://daymet.ornl.gov/static/graphics/TilesV4_Daymet.png)
#'
#' @return Returns character vector with full paths to downloaded files.
#'
#' @examples
#'
#' \dontrun{
#'
#' get_daymet_swe(year = c(2018, 2019),
#'                tile = c(12095, 12096),
#'                out_dir = tempdir())
#' }
#'
#' @export
get_daymet_swe <- function(year,
                           tile,
                           out_dir = "data") {

  # Check if out_dir needs to be created
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Repeat year and tile so paste gets all combinations of year and tile
  years <- rep(year, length(tile))
  tiles <- rep(tile, each = length(year))

  # Create data URL
  durl <- paste0("https://thredds.daac.ornl.gov/",
                 "thredds/fileServer/ornldaac/1328/tiles/",
                 years, "/", tiles, "_", years, "/swe.nc")

  # Create output filename
  ofiles <- paste0(tiles, "_", years, "_swe.nc")
  odir <- file.path(out_dir, ofiles)

  # Pass code to shell for download
  for (i in 1:length(durl)){
    cat("Downloading file", i, "of", length(durl), "... \n\n")
    try(utils::download.file(url = durl[i],
                             destfile = odir[i],
                             mode = "wb"))
    cat("\nFile", i, "of", length(durl),"done!\n\n")
  }

  # Return downloaded file paths
  return(odir)
}

# Example
# get_daymet_swe(year = c(2018, 2019),
#                tile = c(12095, 12096),
#                out_dir = "data")

#' Mosaic Daymet files
#'
#' Mosaic daymet tiles by year and day
#'
#' @param dir `[character]` Directory where Daymet data are stored
#' @param days `[integer = 1:365]` Integer(ish) vector giving the days you want
#' retained in the data. Defaults to all days (1:365). See note in Details.
#' @param out_dir `[character = "data"]` Directory to save mosaicked output.
#' @param format `[character = "raster"]` Output format of raster. Either
#' `"raster"` (.grd) or `"GTiff"` (.tif).
#' @param crop `[Extent = NULL]` Optional. Extent for cropping Daymet rasters.
#' If `NULL` (default), no cropping occurs. `reproject` takes priority over
#' `crop` if both are specified.
#' @param reproject `[Raster* = NULL]` Optional. Raster for reprojecting Daymet
#' rasters. If `NULL` (default), no reprojection occurs. `reproject` takes
#' priority over `crop` if both are specified.
#' @param method `[character = "ngb"]` Method used to compute values if
#' reprojecting raster. Either "ngb" or "bilinear". Ignored if `reproject` is
#' `NULL`.
#' @param verbose `[logical = TRUE]` Determines whether the user will be
#' notified of progress.
#'
#' @details This function takes the tiles downloaded by
#' \code{\link{get_daymet_swe}()} and mosaics them together by year and day.
#' Each Daymet NetCDF file has 365 bands corresponding to days of the year. This
#' function finds all the `*.nc` files for each year in the folder, then
#' determines which bands (days) to load with the argument `days`, so
#' processing is sped up noticeably if you only specify the days you need. The
#' output is then saved in a single-band GeoTIFF labeled with yyyy_day in the
#' filename.
#'
#' A note on Daymet days. During leap years, there are still only 365 bands in
#' the raster. December 31st is omitted during leap years.
#' @export
mosaic_daymet <- function(dir,
                          days = 1:365,
                          out_dir = ".",
                          format = c("raster", "GTiff"),
                          crop = NULL,
                          reproject = NULL,
                          method = "ngb",
                          verbose = TRUE){

  # Check if out_dir needs to be created
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Check `crop` and `reproject`
  if (!is.null(crop)){
    if (!(inherits(crop, "Raster") | inherits(crop, "Extent"))) {
      stop("If 'crop' is not NULL, it must be of class 'Raster*' or 'Extent'.")
    }
  }

  if (!is.null(reproject)){
    if (!(inherits(reproject, "Raster"))) {
      stop("If 'reproject' is not NULL, ",
      "it must be of class 'Raster*'.")
    }
  }

  # Check `method`
  if (!(method %in% c("ngb", "bilinear"))){
    stop("Argument 'method' must be either 'ngb' or 'bilinear'.")
  }

  # Files
  f <- list.files(dir, pattern = ".nc", full.names = TRUE)
  # Basenames
  bn <- basename(f)
  # Get years
  years <- unique(unlist(lapply(strsplit(bn, split = "_", fixed = TRUE),
                  getElement, 2)))
  # Initialize character vector to store filenames for return
  file_names <- character()
  # Mosaic each year
  for (y in years){
    # Files for that year
    fy <- grep(y, f, value = TRUE)
    # Status
    if (verbose) {
      cat(paste0("Year: ", y, "\n"))
    }
    # Days requested
    for (d in days) {
      # Status
      if (verbose) {
        cat(paste0(" ... Day: ", d, "\n"))
      }
      # Load all
      rl <- lapply(fy, raster::raster, band = d)
      # Add mosaic arguments
      rl$fun <- mean #Shouldn't matter b/c tiles do not overlap
      rl$na.rm <- TRUE
      # Mosaic
      m <- do.call(raster::mosaic, rl)
      # Crop or reproject
      if (!is.null(crop) | !is.null(reproject)) {
        m <- crop_or_reproject(r = m,
                               crop = crop,
                               reproject = reproject,
                               method = method)
      }
      # Pad day with 0s for filename
      day <- formatC(d, width = 3, format = "d", flag = "0")
      # Decide extension
      ext <- switch(format[1],
                    raster = ".grd",
                    GTiff = ".tif")
      # Create filename
      fn <- file.path(out_dir, paste0(y, "_", day, "_swe", ext))
      # Save
      raster::writeRaster(m, fn, format = format[1], overwrite = TRUE)
      # Append fn to return later
      file_names <- c(file_names, fn)
    }
  }

  return(file_names)
}

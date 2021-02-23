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

#' @export
mosaic_daymet <- function(dir, out_dir = "."){
  # Files
  f <- list.files(dir, pattern = ".nc", full.names = TRUE)
  # Basenames
  bn <- basename(f)
  # Get years
  years <- unique(unlist(lapply(strsplit(bn, split = "_", fixed = TRUE),
                  getElement, 2)))
  # Mosaic each year
  for (y in years){
    # Files for that year
    fy <- grep(y, f, value = TRUE)
    # Load all
    rl <- lapply(fy, raster::raster)
    # Add mosaic arguments
    rl$fun <- mean
    rl$na.rm <- TRUE
    # Mosaic
    m <- do.call(raster::mosaic, rl)
    # Save
    raster::writeRaster(m, file.path(out_dir, paste0(y, "_swe.grd")))
  }

  return(file.path(out_dir, paste0(years, "_swe.grd")))
}

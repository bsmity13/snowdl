# Helper functions used across multiple scripts.


#' Crop or Reproject a Raster
#'
#' Decides whether to crop or reproject based on arguments.
#'
#' @param r The RasterLayer to crop or reproject.
#' @param crop A Raster* or Extent to crop to.
#' @param reproject A Raster* to reproject to.
#' @param method The method if reprojecting.
#'
#' @details Meant for internal use.
#'
crop_or_reproject <- function(r, crop, reproject, method){
  # If reproject is given, then ignore crop

  if (is.null(reproject)) {
    res <- raster::crop(x = r, y = crop, snap = "out")
  } else {
    res <- raster::projectRaster(from = r,
                                 to = reproject,
                                 method = method)
  }

  return(res)
}

#' Return day-of-year for a date
#'
#' Internal function that calculates day-of-year for a date
#'
#' @details Meant for internal use.
#'
doy <- function(d) {
  j <- format(d, "%j")
  return(as.numeric(j))
}

#' Return year for a date
#'
#' Internal function that calculates year for a date
#'
#' @details Meant for internal use.
#'
yr <- function(d) {
  y <- format(d, "%Y")
  return(as.numeric(y))
}

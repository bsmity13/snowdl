# Functions for downloading SNODAS data

# Process SNODAS dates ----
#' Process `snowdl_dates` for SNODAS
#'
#' Process `snowdl_dates` object for use with SNODAS
#'
#' @param dates `[snowdl_dates]` The object that defines the dates for which to
#' download data. See ?snowdl_dates.
#' @param masked `[logical = TRUE]` Determines whether the downloaded data will
#' be masked or unmasked. See details.
#'
#' @details This function determines URLs to download SNODAS data over HTTP from
#' this site: "https://noaadata.apps.nsidc.org/NOAA/G02158/".
#'
#' On the argument `masked`: SNODAS makes predictions slightly outside of the
#' contiguous United States (unmasked), but crops the predictions to the
#' contiguous US (masked). Note that while masked rasters are available
#' beginning September 2003, unmasked rasters are only available beginning
#' in December 2009.
#'
#' @returns An object of class `SNODAS_dates` to be passed to
#' `download_SNODAS()`.
#'
#' @examples
#'
#' \dontrun{
#'
#' dd <- snowdates_ydoy(2008:2010, 1:3, all_combos = TRUE)
#' dates_SNODAS(dd, masked = TRUE)
#' dates_SNODAS(dd, masked = FALSE)
#'
#' }
#'
#' @export
dates_SNODAS <- function(dates, masked = TRUE) {
  if (!is.snowdl_dates(dates)) {
    stop("Argument 'dates' must be of class 'snowdl_dates'. See ?as_snowdl_dates.")
  }

  # Grab date components
  y <- dates$year
  m <- format(dates$date, "%m")
  mon <- format(dates$date, "%b")
  d <- format(dates$date, "%d")

  # URL and availability depend on 'masked'
  if (masked) {
    # Base URL
    base_url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked/"

    # Filename
    fn <- paste0("SNODAS_", y, m, d, ".tar")

    # Check dates (masked available starting 2003-09-30)
    dates$available <- dates$date >= as.Date("2003-09-30")
  } else {
    # Base URL
    base_url <- "https://noaadata.apps.nsidc.org/NOAA/G02158/unmasked/"

    # Filename
    fn <- paste0("SNODAS_unmasked_", y, m, d, ".tar")

    # Check dates (unmasked available starting 2009-12-09)
    dates$available <- dates$date >= as.Date("2009-12-09")
  }


  # Construct URL
  dates$url <- paste0(base_url, y, "/", m, "_", mon, "/", fn)
  # Overwrite if data are unavailable
  dates$url[dates$available == FALSE] <- NA

  # Set class
  class(dates) <- c("SNODAS_dates", class(dates))

  # Return
  return(dates)
}

# Function to download data ----

#' Download SNODAS
#'
#' Downloads SNODAS data for dates in a `snowdl_dates` object
#'
#' @param dates `[SNODAS_dates]` The object that defines the dates for which to
#' download data. See ?dates_SNODAS.
#' @param out_dir `[character = "."]` The directory in which to save downloaded
#' file.
#' @param overwrite `[logical = FALSE]` If data for particular dates exist in
#' `out_dir`, should it be overwritten?
#'
#' @details This function downloads SNODAS data over HTTP from
#' this site: "https://noaadata.apps.nsidc.org/NOAA/G02158/".
#'
#' @returns An object of class `SNODAS_download` to be passed to
#' `unpack_SNODAS()`.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Just two dates
#' dts <- as_snowdl_dates(as.Date("2020-02-02") + 1:2)
#'
#' # Process dates
#' SNODAS_dts <- dates_SNODAS(dts)
#'
#' # Download to temporary directory
#' res <- download_SNODAS(SNODAS_dts, out_dir = tempdir())
#'
#' # Include a date that's unavailable
#' dt2 <- as_snowdl_dates(as.Date(c("2001-02-02", "2010-02-02")))
#' SNODAS_dt2 <- dates_SNODAS(dt2)
#' res2 <- download_SNODAS(SNODAS_dt2, out_dir = tempdir())
#'
#' }
#'
#' @export
download_SNODAS <- function(dates, out_dir = ".", overwrite = FALSE) {

  # Check that 'dates' is correct class
  if(!methods::is(dates, "SNODAS_dates")) {
    stop("'dates' must be processed with 'dates_SNODAS()'.")
  }

  # Create out_dir if necessary
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Get filename from url
  fn <- sapply(strsplit(dates$url, "/", fixed = TRUE), function(x) {
    if (length(x) == 1) {
      return(NA)
    } else {
      return(x[[9]])
    }
  })

  # Output file
  dates$download <- ifelse(is.na(fn), NA, file.path(out_dir, fn))

  # Download
  dates$download_status <- NA

  for (i in 1:nrow(dates)) {
    if (!is.na(dates$url[i])) {
      # Is there a file that shouldn't be overwritten?
      if(file.exists(dates$download[i]) & !overwrite) {
        dates$download_status[i] <- "exists"
      } else { # The file doesn't exist or should be overwritten
        # Download
        try(res <- utils::download.file(url = dates$url[i],
                                        destfile = dates$download[i],
                                        mode = "wb"))
        dates$download_status[i] <- ifelse(res == 0, "success", "fail")
      }
    } else { # The data are unavailable

    }
  }

  # Set class
  class(dates) <- c("SNODAS_download", class(dates))

  return(dates)
}


# Unpack all SNODAS tarballs (old workflow) ----
#' Unpack all SNODAS tarballs
#'
#' Unpacks all SNODAS tarballs in a directory
#'
#' @param tar_dir `[character = "."]` The directory from which to unpack all
#' SNODAS tarballs
#' @param out_dir `[character = "."]` The directory where outputs will be saved
#' @param rm_tar `[logical = FALSE]` Should tarballs be deleted after unpacking?
#'
#' @details This function unpacks tarballs and returns SWE and snow depth files
#' with new names in a user-defined directory.
#'
#' This is a remnant of the previous (v0.1.0) workflow that takes just a
#' directory as an input. It remains available for convenience, but the
#' current (v1.0.0) workflow tracks the files in a custom `data.frame` and
#' users should use the function TBD.
#'
#' @examples
#' \dontrun{
#'
#' # Just two dates
#' dts <- as_snowdl_dates(as.Date("2020-02-02") + 1:2)
#'
#' # Process dates
#' SNODAS_dts <- dates_SNODAS(dts)
#'
#' # Download to temporary directory
#' dd <- tempdir()
#' res <- download_SNODAS(SNODAS_dts, out_dir = dd)
#'
#' # Unpack
#' unpack_dir_SNODAS(dd)
#'
#' }
#' @export
unpack_dir_SNODAS <- function(tar_dir = ".", out_dir = "data", rm_tar = FALSE) {

  # Check if out_dir needs to be created
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # List SNODAS tarballs in directory
  tars <- list.files(tar_dir,
                     pattern = utils::glob2rx("SNODAS*.tar"),
                     full.names = TRUE)

  # If there are no tarballs here
  if (length(tars) == 0) {
    stop("There are no SNODAS tarballs in 'tar_dir'.")
  }

  # Create directories for all unpacked tars
  dirs <- file.path(out_dir, gsub(".tar", "", basename(tars)))
  lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)

  # Loop over tars and unpack
  for (i in 1:length(tars)) {

    # Unpacked directory
    udir <- dirs[i]

    #Un-tar
    utils::untar(tars[i], exdir = udir)

    # Possibly remove tarball
    if (rm_tar) {
      file.remove(tars[i])
    }
  }
  return(dirs)
}

# Unpack SNODAS tarballs (new workflow) ----
#' Unpack SNODAS tarballs
#'
#' Unpacks SNODAS tarballs from a `SNODAS_download` object
#'
#' @param dates `[SNODAS_download]` The object returned by `download_SNODAS()`
#' @param out_dir `[character = "."]` The directory where outputs will be saved
#' @param rm_tar `[logical = FALSE]` Should tarballs be deleted after unpacking?
#'
#' @details This function unpacks tarballs and returns SWE and snow depth files
#' with new names in a user-defined directory.
#'
#' This is the preferred unpacking function for v1.0.0.
#'
#' @seealso [unpack_dir_SNODAS()] for an older approach (v0.1.0) that unpacks
#' all tarballs in a directory without a `SNODAS_download` object.
#'
#' @examples
#' \dontrun{
#'
#' # Three dates; one is unavailable
#' dts <- as_snowdl_dates(as.Date(c("2000-02-02", "2010-02-02", "2020-02-02")))
#'
#' # Process dates
#' SNODAS_dts <- dates_SNODAS(dts)
#'
#' # Download to temporary directory
#' dd <- tempdir()
#' dl <- download_SNODAS(SNODAS_dts, out_dir = dd)
#'
#' # Unpack
#' res <- unpack_SNODAS(dl, out_dir = dd)
#'
#' }
#' @export
unpack_SNODAS <- function(dates, out_dir = "data", rm_tar = FALSE) {

  # Check that 'dates' object is correct class
  if (!methods::is(dates, "SNODAS_download")) {
    stop("An object of class 'SNODAS_download' ",
         "must be passed to argument 'dates'. See ?download_SNODAS.")
  }

  # Check if out_dir needs to be created
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Create directories for all unpacked tars
  dates$unpacked <- file.path(out_dir, gsub(".tar", "",
                                            basename(dates$download)))
  dates$unpacked[which(!dates$available)] <- NA
  lapply(stats::na.omit(dates$unpacked), dir.create,
         showWarnings = FALSE, recursive = TRUE)
  dates$unpack_status <- NA

  # Loop over successfully downloaded tars and unpack
  for (i in which(dates$download_status %in% c("success", "exists"))) {

    # Tarball and unpacking directory
    tar <- dates$download[i]
    udir <- dates$unpacked[i]

    #Un-tar
    utils::untar(tar, exdir = udir)

    # Possibly remove tarball
    if (rm_tar) {
      file.remove(tar)
      # If we remove the tarball, we want the function to know in the future
      # not to try to unpack it.
      dates$unpack_status[i] <- "unpacked_removed"
    } else {
      dates$unpack_status[i] <- "unpacked_available"
    }
  }

  # Change class of 'dates'
  class(dates) <- c("SNODAS_unpacked", class(dates))

  return(dates)
}


# Rasterize SNODAS files ----
#' Rasterize SNODAS files
#'
#' Saves as raster already untarred files in SNODAS directory
#'
#' @param dates `[SNODAS_unpacked]` Object returned by `unpack_SNODAS()`
#' @param out_dir `[character = "data"]` Directory where rasters should be
#' saved
#' @param rm_data `[logical = FALSE]` Delete untarred files after saving to
#' raster?
#' @param format `[character = "GTiff"]` Output format of raster. Only `"GTiff"`
#' (.tif) is currenty supported.
#' @param crop `[SpatRaster, SpatExtent = NULL]` Optional. Template for cropping
#' SNODAS rasters. If `NULL` (default), no cropping occurs. `reproject` takes
#' priority over `crop` if both are specified.
#' @param reproject `[SpatRaster = NULL]` Optional. Raster for reprojecting
#' SNODAS rasters. If `NULL` (default), no reprojection occurs. `reproject`
#' takes priority over `crop` if both are specified.
#' @param method `[character = "near"]` Method used to compute values if
#' reprojecting raster. See ?terra::project for options. Ignored if `reproject`
#' is `NULL`.
#' @param verbose `[logical = TRUE]` Determines whether the user will be
#' notified of progress in the console.
#'
#' @returns Returns a `data.frame` of class `TBD` with metadata about each
#' raster. As a side-effect, it also saves the rasters to disk, with one
#' file for each date. Each file has 3 layers, which should retain their
#' names, but are (1) SWE in m (`swe`), (2) Snow Depth in m (`depth`), and (3)
#' Snow Pack Average Temperature in K (`temp`).
#'
#' @seealso [`SNODAS_pc`]`()` for the full names and units of each layer
#'
#'
#' @examples
#'
#' \dontrun{
#' # Three dates; one is unavailable
#' dts <- as_snowdl_dates(as.Date(c("2000-02-02", "2010-02-02", "2020-02-02",
#'                                   "2017-03-30")))
#'
#' # Process dates
#' SNODAS_dts <- dates_SNODAS(dts)
#'
#' # Download to temporary directory
#' dd <- tempdir()
#' dl <- download_SNODAS(SNODAS_dts, out_dir = dd)
#'
#' # Unpack
#' up <- unpack_SNODAS(dl, out_dir = dd)
#'
#' # Rasterize
#' res <- rasterize_SNODAS(up, out_dir = dd)
#'
#' }
#'
#' @export
rasterize_SNODAS <- function(dates,
                             out_dir = "data/geo",
                             rm_data = FALSE,
                             format = "GTiff",
                             crop = NULL,
                             reproject = NULL,
                             method = "near",
                             verbose = TRUE) {

  # Create out_dir if necessary
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Check `crop` and `reproject`
  if (!is.null(crop)){
    if (!(inherits(crop, "SpatRaster") | inherits(crop, "SpatExtent"))) {
      stop("If 'crop' is not NULL, it must be of class 'SpatRaster' or 'SpatExtent'.")
    }
  }

  if (!is.null(reproject)){
    if (!(inherits(reproject, "SpatRaster") |
          inherits(reproject, "character"))) {
      stop("If 'reproject' is not NULL, ",
           "it must be of class 'SpatRaster' or 'character'.")
    }
  }

  # Check `method`
  if (!(method %in% c("near", "bilinear", "cubic", "cubicspline", "lanczos"))) {
    stop("Argument 'method' must be either 'ngb' or 'bilinear'.")
  }

  # Check file type
  if (!(format %in% c("GTiff"))) {
    stop("Argument 'format' must be 'GTiff' (others may be implemented soon).")
  }

  # Get product codes
  PC <- SNODAS_pc()

  # Initialize 'raster' column
  dates$raster <- NA

  # Loop over rows of 'dates' for which the file has been unpacked
  for (i in which(!is.na(dates$unpack_status))) {

    if (verbose) {
      # How many rows?
      rws <- sum(!is.na(dates$unpack_status))
      # Which one?
      ii <- which(i == which(!is.na(dates$unpack_status)))
      cat("Processing raster", ii, "of", rws, " \n")
      cat(" ... loading data \n")
    }

    # Loop over product codes in SNODAS data
    # Store each as an element of a list
    r <- list()
    for (j in 1:nrow(PC)) {
      pc <- PC$pc[j]
      nm <- PC$parm[j]
      sf <- PC$scale_factor[j]

      # Path to parameter files
      p.txt.gz <- list.files(dates$unpacked[i],
                             pattern = utils::glob2rx(paste0("*", pc, "*.txt.gz")),
                             full.names = TRUE,
                             recursive = TRUE)
      p.dat.gz <- list.files(dates$unpacked[i],
                             pattern = utils::glob2rx(paste0("*", pc, "*.dat.gz")),
                             full.names = TRUE,
                             recursive = TRUE)
      # Unzip (returns path to unzipped files)
      p.txt <- R.utils::decompressFile.default(p.txt.gz,
                                               ext = "gz",
                                               FUN = gzfile,
                                               overwrite = TRUE,
                                               remove = FALSE)
      p.dat <- R.utils::decompressFile.default(p.dat.gz,
                                               ext = "gz",
                                               FUN = gzfile,
                                               overwrite = TRUE,
                                               remove = FALSE)

      # Note: there is an apparent bug in these files from
      # 2017-03-23 to 2017-04-03 where the header file has
      # an incorrect version and GDAL will not recognize it.
      problem_ds <- seq.Date(as.Date("2017-03-23"),
                             as.Date("2017-04-03"),
                             by = "1 day")
      if (dates$date[i] %in% problem_ds) {
        # Fix header
        xfun::gsub_file(file = p.txt,
                        pattern = "NOHRSC GIS/RS raster file v1.0",
                        replacement = "NOHRSC GIS/RS raster file v1.1",
                        fixed = TRUE)
      }

      # Read raster
      r[[j]] <- terra::rast(p.txt)
      names(r[[j]]) <- nm

      # Apply scale factor
      r[[j]] <- r[[j]]/sf
    }

    # Combine the list into a single raster
    rr <- terra::rast(r)

    # Crop or reproject
    if (!is.null(crop) | !is.null(reproject)) {

      if (verbose) {
        cat(" ... cropping or reprojecting \n")
      }

      rr <- crop_or_reproject(r = rr,
                              crop = crop,
                              reproject = reproject,
                              method = method)
    }

    if (verbose) {
      cat(" ... saving \n")
    }
    # Write
    # Base filename
    if (format == "GTiff") {
      fn <- paste0(basename(dates$unpacked[i]), ".tif")
    }
    # Full filename
    dates$raster[i] <- file.path(out_dir, fn)
    terra::writeRaster(rr,
                       filename = dates$raster[i],
                       filetype = format,
                       overwrite = TRUE)
    # Delete
    if (rm_data) {
      if (verbose) {
        cat(" ... removing data \n")
      }
      # Folder containing data
      f <- dates$unpacked[i]
      # Delete
      unlink(f, recursive = TRUE)
      # Update status
      dates$unpack_status[i] <- "unpacked_deleted"
    }

  }


  if (verbose) {
    cat("Done!\n")
  }

  # Update the class of 'dates
  class(dates) <- c("SNODAS_rasters", class(dates))
  return(dates)
}

# SNODAS product codes ----
#' Display SNODAS Product Codes
#'
#' Displays state variables available in SNODAS data
#'
#' @seealso See Table 1 of SNODAS user guide at
#' https://nsidc.org/data/g02158/versions/1
#'
#' @examples
#' SNODAS_pc()
#'
#'
#' @export
SNODAS_pc <- function() {
  # Display product codes in SNODAS data
  # See Table 1 of user guide at https://nsidc.org/data/g02158/versions/1

  # Reporting state variables only
  pc <- data.frame(pc = c(1034, 1036, 1038),
                   parameter = c("SWE", "Snow Depth", "Snow Pack Average Temperature"),
                   parm = c("swe", "depth", "temp"),
                   units = c("m", "m", "K"),
                   scale_factor = c(1000, 1000, 1))
  return(pc)
}

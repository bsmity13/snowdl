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
#' @details TBD
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
#' @param date `[SNODAS_dates]` The object that defines the dates for which to
#' download data. See ?dates_SNODAS.
#' @param out_dir `[character = "."]` The directory in which to save downloaded
#' file.
#' @param overwrite `[logical = FALSE]` If data for particular dates exist in
#' `out_dir`, should it be overwritten?
#' @param masked `[logical = TRUE]` Determines whether the downloaded data will
#' be masked or unmasked. See details.
#'
#' @details This function downloads SNODAS data over HTTP from
#' this site: "https://noaadata.apps.nsidc.org/NOAA/G02158/".
#'
#' On the argument `masked`: SNODAS makes predictions slightly outside of the
#' contiguous United States (unmasked), but crops the predictions to the
#' contiguous US (masked). Note that while masked rasters are available
#' beginning September 2003, unmasked rasters are only available beginning
#' in December 2009.
#'
#' This function is called by wrapper function **TBD**.
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
  if(!is(dates, "SNODAS_dates")) {
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
  dates$status <- NA

  for (i in 1:nrow(dates)) {
    if (!is.na(dates$url[i])) {
      # Is there a file that shouldn't be overwritten?
      if(file.exists(dates$download[i]) & !overwrite) {
        dates$status[i] <- "exists"
      } else { # The file doesn't exist or should be overwritten
        # Download
        try(res <- utils::download.file(url = dates$url[i],
                                        destfile = dates$download[i],
                                        mode = "wb"))
        dates$status[i] <- ifelse(res == 0, "success", "fail")
      }
    } else { # The data are unavailable

    }
  }

  # Set class
  class(dates) <- c("SNODAS_download", class(dates))

  return(dates)
}


# Function to unpack SNODAS tarballs ----

#' Unpack all SNODAS tarballs
#'
#' Unpacks all SNODAS tarballs in a directory
#'
#' @param tar_dir `[character = "."]` The directory from which to unpack all
#' SNODAS tarballs
#' @param out_dir `[character = "."]` The directory where outputs will be saved
#' @param rm_tar `[logical = TRUE]` Should tarball be deleted after unpacking?
#'
#' @details This function unpacks tarballs and returns SWE and snow depth files
#' with new names in a user-defined directory.
#'
#' Will probably end up getting called by a wrappper. **TBD**
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
unpack_dir_SNODAS <- function(tar_dir = ".", out_dir = "data", rm_tar = TRUE) {

  # Check if out_dir needs to be created
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # List SNODAS tarballs in directory
  tars <- list.files(tar_dir,
                     pattern = utils::glob2rx("SNODAS*.tar"),
                     full.names = TRUE)

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

#' Rasterize SNODAS files
#'
#' Saves as raster already untarred files in SNODAS directory
#'
#' @param data_dir `[character = "data"]` Directory where `SNODAS_*` folders are
#' located
#' @param out_dir `[character = "data"]` Directory where rasters should be
#' saved
#' @param rm_data `[logical = TRUE]` Delete untarred files after saving to
#' raster?
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
#' @details Note that if this function exits with an error, it will most
#' likely have already untarred the compressed files that were downloaded.
#' That will typically cause the function `rasterize_SNODAS()` to fail when
#' run a second time. This is not a desirable outcome, and it is on my to-do
#' list to fix this.
#'
#' More details required. **TBD**
#'
#' @export
rasterize_SNODAS <- function(data_dir = "data",
                             out_dir = "data/geo",
                             rm_data = TRUE,
                             format = c("raster", "GTiff"),
                             crop = NULL,
                             reproject = NULL,
                             method = "ngb",
                             verbose = TRUE) {

  # Create out_dir if necessary
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
  if (!(method %in% c("ngb", "bilinear"))) {
    stop("Argument 'method' must be either 'ngb' or 'bilinear'.")
  }

  # Check file type
  if (!(format %in% c("raster", "GTiff"))) {
    stop("Argument 'format' must be either 'raster' or 'GTiff'.")
  }

  if (verbose) {
    cat("Extracting *.gz files ... \n")
  }

  # Get SWE files in all directories
  swe.txt.gz <- list.files(data_dir,
                           pattern = utils::glob2rx('*ssmv11034tS*.txt.gz'),
                           full.names = TRUE,
                           recursive = TRUE)
  swe.dat.gz <- gsub(".txt.", ".dat.", swe.txt.gz, fixed = TRUE)
  # Unzip (returns unzipped filenames)
  swe.txt <- unlist(lapply(swe.txt.gz, function(x) {
    R.utils::decompressFile.default(x,
                                    ext = "gz",
                                    FUN = gzfile,
                                    overwrite = TRUE,
                                    remove = FALSE)
  }))
  swe.dat <- unlist(lapply(swe.dat.gz, function(x) {
    R.utils::decompressFile.default(x,
                                    ext = "gz",
                                    FUN = gzfile,
                                    overwrite = TRUE,
                                    remove = FALSE)
  }))

  # Get snow depth files in all directories
  dep.txt.gz <- list.files(data_dir,
                           pattern = utils::glob2rx('*ssmv11036tS*.txt.gz'),
                           full.names = TRUE,
                           recursive = TRUE)
  dep.dat.gz <- gsub(".txt.", ".dat.", dep.txt.gz, fixed = TRUE)
  # Unzip (returns unzipped filenames)
  dep.txt <- unlist(lapply(dep.txt.gz, function(x) {
    R.utils::decompressFile.default(x,
                                    ext = "gz",
                                    FUN = gzfile,
                                    overwrite = TRUE,
                                    remove = FALSE)
  }))
  dep.dat <- unlist(lapply(dep.dat.gz, function(x) {
    R.utils::decompressFile.default(x,
                                    ext = "gz",
                                    FUN = gzfile,
                                    overwrite = TRUE,
                                    remove = FALSE)
  }))

  # Extract date string from filenames
  ds <- substr(x = basename(swe.txt), start = 28, stop = 35)

  # Initialize output filenames
  ffn <- character()

  if (verbose) {
    cat("Saving rasters: \n")
  }

  # Loop through each date, load SWE and depth rasters, (crop), write, (delete)
  for (i in 1:length(ds)) {
    # Note: there is an apparent bug in these files from
    # 2017-03-23 to 2017-04-03 where the header file has
    # an incorrect version and GDAL will not recognize it.
    problem_ds <- seq.Date(as.Date("2017-03-23"),
                           as.Date("2017-04-03"),
                           by = "1 day")
    if (as.Date(ds[i], format = "%Y%m%d") %in% problem_ds) {
      # Fix swe
      xfun::gsub_file(file = swe.txt[i],
                      pattern = "NOHRSC GIS/RS raster file v1.0",
                      replacement = "NOHRSC GIS/RS raster file v1.1",
                      fixed = TRUE)
      # Fix depth
      xfun::gsub_file(file = dep.txt[i],
                      pattern = "NOHRSC GIS/RS raster file v1.0",
                      replacement = "NOHRSC GIS/RS raster file v1.1",
                      fixed = TRUE)
    }
    if (verbose) {
      cat("    ", as.character(as.Date(ds[i], format = "%Y%m%d")), "\n")
    }
    # Load both layers
    r <- raster::stack(swe.txt[i], dep.txt[i])
    # Rename
    names(r) <- c("SWE", "depth")
    # Crop or reproject
    if (!is.null(crop) | !is.null(reproject)) {
      r <- crop_or_reproject(r = r,
                             crop = crop,
                             reproject = reproject,
                             method = method)
    }
    # Write
    # Base filename
    fn <- switch(format[1],
                 raster = paste0("SNODAS_", ds[i], ".grd"),
                 GTiff = paste0("SNODAS_", ds[i], ".tif"))
    # Full filename
    ffn[i] <- file.path(out_dir, fn)
    raster::writeRaster(r, filename = ffn[i],
                        format = format[1],
                        overwrite = TRUE)
    # Delete
    if (rm_data) {
      # Folder containing data
      # Get it by removing "/filename.txt"
      f <- gsub(paste0("/", basename(swe.txt[i])), "", swe.txt[i])
      # Delete
      unlink(f, recursive = TRUE)
    }
  }

  if (verbose) {
    cat("Done!\n")
  }

  # Return full filenames at the end
  return(ffn)
}

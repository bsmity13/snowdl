# Functions for downloading SNODAS data

# Function to download data for one date ----

#' Download SNODAS
#'
#' Downloads SNODAS data for a single date
#'
#' @param date `[date, POSIXt]` The (1) date for which to download data.
#' @param out_dir `[character = "."]` The directory in which to save downloaded
#' file.
#' @param overwrite `[logical = FALSE]` If data for `date` exists in `out_dir`,
#' should it be overwritten?
#'
#' @details This function downloads SNODAS data over FTP from
#' this site: "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/".
#'
#' This function is called by wrapper function **TBD**.
#'
#' @examples
#'
#' \dontrun{
#'
#' download_SNODAS(as.Date("2020-02-02"), out_dir = tempdir())
#'
#' }
#'
#' @export
download_SNODAS <- function(date, out_dir = ".", overwrite = FALSE) {

  # Base URL
  base_url <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/"

  # Grab date components
  y <- format(date, "%Y")
  m <- format(date, "%m")
  mon <- format(date, "%b")
  d <- format(date, "%d")

  # Filename
  fn <- paste0("SNODAS_", y, m, d, ".tar")

  # Output file
  of <- file.path(out_dir, fn)

  # If file doesn't exist or should be overwritten
  if(!file.exists(of) | overwrite){
    # Construct URL
    u <- paste0(base_url, y, "/", m, "_", mon, "/", fn)

    # Download
    try(utils::download.file(url = u,
                  destfile = of,
                  mode = "wb"))
  } else {
    warning("Did not download file ", fn)
  }

  # No return (called for side-effect = download file)

}


# Function to unpack SNODAS tarballs ----

#' Unpack SNODAS tarballs
#'
#' Unpacks all SNODAS tarballs in a directory
#'
#' @param tar_dir `[character = "."]` The directory from which to unpack all
#' SNODAS tarballs
#' @param new_dir `[character = "."]` The directory where outputs will be saved
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
#' dd <- tempdir()
#'
#' download_SNODAS(as.Date("2020-02-02"), out_dir = dd)
#' unpack_SNODAS(dd)
#'
#' }
#' @export
unpack_SNODAS <- function(tar_dir = ".", new_dir = "data", rm_tar = TRUE) {

  # Check if new_dir needs to be created
  if (!dir.exists(new_dir)) {
    dir.create(new_dir)
  }

  # List SNODAS tarballs in directory
  tars <- list.files(tar_dir,
                     pattern = utils::glob2rx("SNODAS*.tar"))

  # Create directories for all unpacked tars
  dirs <- file.path(new_dir, gsub(".tar", "", tars))
  lapply(dirs, dir.create, showWarnings = FALSE)

  # Loop over tars and unpack
  for (i in 1:length(tars)) {

    # Unpacked directory
    udir <- file.path(tar_dir, dirs[i])

    #Un-tar
    utils::untar(tars[i], exdir = udir)

    # Possibly remove tarball
    if (rm_tar) {
      file.remove(tars[i])
    }
  }
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
#' @param extent `[Extent = NULL]` Optional. Extent for cropping SNODAS rasters.
#' If `NULL` (default), no cropping occurs.
#' @param verbose `[logical = TRUE]` Determines whether the user will be
#' notified of progress.
#'
#' @details Blah blah. **TBD**
#'
#' @export
rasterize_SNODAS <- function(data_dir = "data",
                             out_dir = "data/geo",
                             rm_data = TRUE,
                             format = c("raster", "GTiff"),
                             extent = NULL,
                             verbose = TRUE) {

  # Create out_dir if necessary
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  if(verbose) {
    cat("Extracting *.gz files ... \n")
  }

  # Get SWE files in all directories
  swe.txt.gz <- list.files(data_dir,
                     pattern = utils::glob2rx('*ssmv11034tS*.txt.gz'),
                     full.names = TRUE,
                     recursive = TRUE)
  swe.dat.gz <- gsub(".txt.", ".dat.", swe.txt.gz, fixed = TRUE)
  # Unzip (returns unzipped filenames)
  swe.txt <- unlist(lapply(swe.txt.gz, R.utils::gunzip))
  swe.dat <- unlist(lapply(swe.dat.gz, R.utils::gunzip))

  # Get snow depth files in all directories
  dep.txt.gz <- list.files(data_dir,
                           pattern = utils::glob2rx('*ssmv11036tS*.txt.gz'),
                           full.names = TRUE,
                           recursive = TRUE)
  dep.dat.gz <- gsub(".txt.", ".dat.", dep.txt.gz, fixed = TRUE)
  # Unzip (returns unzipped filenames)
  dep.txt <- unlist(lapply(dep.txt.gz, R.utils::gunzip))
  dep.dat <- unlist(lapply(dep.dat.gz, R.utils::gunzip))

  # Extract date string from filenames
  ds <- substr(x = basename(swe.txt), start = 28, stop = 35)

  # Initialize output filenames
  ffn <- character()

  if (verbose) {
    cat("Saving rasters: \n")
  }

  # Loop through each date, load SWE and depth rasters, (crop), write, (delete)
  for (i in 1:length(ds)) {
    if (verbose) {
      cat("    ", as.character(as.Date(ds[i], format = "%Y%m%d")), "\n")
    }
    # Load both layers
    r <- raster::stack(swe.txt[i], dep.txt[i])
    # Rename
    names(r) <- c("SWE", "depth")
    # Crop
    if (!is.null(extent)) {
      r <- raster::crop(r, extent, snap = "out")
    }
    # Write
    # Base filename
    fn <- switch(format[1],
                 raster = paste0("SNODAS_", ds[i], ".grd"),
                 GTiff = paste0("SNODAS_", ds[i], ".tif"))
    # Full filename
    ffn[i] <- file.path(out_dir, fn)
    raster::writeRaster(r, filename = ffn[i],
                        format = format[1])
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

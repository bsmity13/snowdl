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
#' \dontrun{
#' download_SNODAS(as.Date("2020-02-02"), out_dir = tempdir())
#' }
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

# Example
# unpack_SNODAS()
unpack_SNODAS <- function(tar_dir = ".", rm_tar = TRUE) {

  # List tarballs in directory
  tars <- list.files(tar_dir, pattern = ".tar")

  # Create directories for all unpacked tars
  dirs <- gsub(".tar", "", tars)
  lapply(dirs, dir.create, showWarnings = FALSE)

  # Loop over tars and unpack
  for (i in 1:length(tars)) {
    #Un-tar
    utils::untar(tars[i], exdir = file.path(tar_dir, dirs[i]))

    # Possibly remove tarball
    if (rm_tar) {
      file.remove(tars[i])
    }
  }
}


# Function to load snow depth ----
# # Example
# x <- load_SNODAS_depth(SNODAS_dir = "SNODAS_20200202")
#
# raster::plot(x)

load_SNODAS_depth <- function(SNODAS_dir) {
  # Depth file
  # Text header
  th <- list.files(path = SNODAS_dir,
                   pattern = utils::glob2rx('*ssmv11036tS*.txt.gz'),
                   full.names = TRUE)
  # Data
  d <- list.files(path = SNODAS_dir,
                   pattern = utils::glob2rx('*ssmv11036tS*.dat.gz'),
                   full.names = TRUE)

  # Unzip gz files
  R.utils::gunzip(th)
  R.utils::gunzip(d)

  # Unzipped header
  uh <- gsub(".gz", "", th)

  # Load raster
  r <- raster::raster(uh)

  # Name layer
  names(r) <- "snow_depth"

  # Return
  return(r)
}



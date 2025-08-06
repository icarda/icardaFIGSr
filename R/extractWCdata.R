#' @title Extracting historical climate data from WorldClim 2.1
#' @description extractWCdata returns a data frame based on specified climatic variables.
#' This function modifies the \href{https://github.com/rspatial/raster/blob/master/R/getData.R}{getData function from the raster R package} to extract world climate data of version 2.1 instead of version 1.4.
#' @param sites object of class "data.frame" with coordinates of sites from which to extract data.
#' @param long character. Name of column from \code{sites} with longitude.
#' @param lat character. Name of column from \code{sites} with latitude.
#' @param res numeric. Spatial resolution. Default 2.5.
#' @param var character. Climatic variable(s) to be extracted: 'tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'.
#' @return An object of class "data.frame" with specified climatic variables for coordinates in \code{sites}.
#' @details A grid can be created with any particular coordinates and used as input for \code{sites} (see section 'Examples'). \code{extractWCdata} will use the given coordinates to extract data from the WorldClim 2.1 database.
#' The extracted data will most likely contain NAs for sites where climate data is not available. These should be removed or imputed before using the data to make predictions.
#' @author Zakaria Kehel, Fawzy Nawar, Bancy Ngatia, Khadija Aouzal, Chafik Analy
#' @examples
#' \dontrun{
#'  # Create grid
#'  long <- seq(-16, 115, length = 3)
#'  lat <- seq(25, 59, length = 3)
#'  sf <- expand.grid(x = sp1, y = sp2)
#' 
#'  # Extract data using grid
#'  sp.df0 <- extractWCdata(sf, long = 'long', lat = 'lat', var = 'bio')
#'  sp.df <- na.omit(sp.df0)
#'  }
#' @name extractWCdata
#' @importFrom sf st_as_sf
#' @importFrom raster extract
#' @importFrom utils browseURL
#' @export

extractWCdata <- function(sites, long, lat, var, res = 2.5) {
  
  # Increase timeout to 10 minutes (600 seconds)
  options(timeout = max(600, getOption("timeout")))
  
  # Validate 'sites' data frame
  if (!is.data.frame(sites) || !all(c(long, lat) %in% colnames(sites))) {
    stop("Sites must be a data frame with 'lat' and 'long' columns")
  }
  
  # Ensure latitude and longitude are numeric
  if (!is.numeric(sites[[lat]]) || !is.numeric(sites[[long]])) {
    stop("Latitude and Longitude must be numeric.")
  }
  
  # Remove records with NA coordinates
  sites <- sites[!is.na(sites[[long]]) & !is.na(sites[[lat]]), ]
  
  # Convert to sf object
  sf_sites <- sf::st_as_sf(sites, coords = c(long, lat), crs = 4326)
  
  # Loop over climatic variables to extract data
  for (ivar in var) {
    rasterfile <- tryCatch({
      .getRasterData(var = ivar, res = res)
    }, error = function(e) {
      message("Automatic download failed for '", ivar, "' from the primary source.")
      message("Attempting fallback download...")
      
      # Fallback to download specific variable from fallback URL
      fallback_url <- paste0("https://www.worldclim.org/data/worldclim21.html#", ivar, "_", res, "m")
      .downloadFallbackFiles(ivar, res, fallback_url)
      
      # Attempt to load the downloaded files again after fallback
      rasterfile <- raster::stack(list.files(paste0(getwd(), "/worldclim/vars/", ivar), pattern = "\\.tif$", full.names = TRUE))
      
      if (length(rasterfile) == 0) {
        stop("No valid files found. Please download from the fallback URL.")
      }
      
      return(rasterfile)
    })
    
    if (!is.null(rasterfile)) {
      for (i in 1:length(names(rasterfile))) {
        f.name <- names(rasterfile)[i]
        var.name <- sub(paste(".*", res, "m_", sep = ''), "", f.name)
        
        # Extract raster values to sf object
        extracted_values <- raster::extract(rasterfile[[i]], sf::st_coordinates(sf_sites), method = 'bilinear')
        sites[, var.name] <- extracted_values
      }
    }
  }
  
  return(sites)
}

# Helper function for downloading and extracting raster data
.getRasterData <- function(var, res) {
  stopifnot(var %in% c('tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'))
  
  path <- paste0(getwd(), "/WorldClim_", res, "/")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  primary_url <- paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_", res, "m_", var, ".zip")
  
  zip <- paste0("wc2.1_", res, "m_", var, ".zip")
  zipfile <- paste0(path, zip)
  
  if (!file.exists(zipfile)) {
    .download(primary_url, zipfile)
    if (!file.exists(zipfile)) {
      stop("Unable to download the file. Attempting fallback download.")
    }
    utils::unzip(zipfile, exdir = dirname(zipfile))
  }
  
  files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)
  st <- raster::stack(files)
  raster::projection(st) <- "+proj=longlat +datum=WGS84"
  return(st)
}

# Download function with error handling
.download <- function(url, filename, overwrite = FALSE) {
  fn <- tempfile(fileext = '.download')
  success <- FALSE
  
  tryCatch({
    res <- utils::download.file(url = url, destfile = fn, method = "libcurl", quiet = FALSE, mode = "wb", cacheOK = TRUE, timeout=600)
    if (res == 0) {
      success <- TRUE
    }
  }, error = function(e) {
    message("Download failed: ", e$message)
    success <- FALSE
  })
  
  if (success) {
    if (file.exists(filename) && !overwrite) {
      stop("File already exists and overwrite is set to FALSE.")
    }
    
    tryCatch({
      if (!file.rename(fn, filename)) {
        if (!file.copy(fn, filename, overwrite = overwrite)) {
          stop("Failed to copy the file.")
        }
      }
      
      if (file.exists(fn)) {
        file.remove(fn)
      }
    }, error = function(e) {
      stop("An error occurred: ", e$message)
    })
  } else {
    stop("Could not download the file. Please manually download it.")
  }
}

# Fallback download function to fetch specific variable files
.downloadFallbackFiles <- function(var, res, fallback_url) {
  # Fallback download logic, using the structure of the fallback URL
  message("Attempting to download variable '", var, "' from fallback URL: ", fallback_url)
  
  # Define local directories to save the downloaded files
  var_path <- paste0(getwd(), "/worldclim/", var, "/")
  dir.create(var_path, showWarnings = FALSE, recursive = TRUE)
  
  # Simulating download by telling user to manually download from URL
  message("Please download the necessary files from the fallback URL and place them in: ", var_path)
  browseURL(fallback_url)  # Automatically opens the fallback URL in the default browser
}

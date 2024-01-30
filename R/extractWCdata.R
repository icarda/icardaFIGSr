#' @title Extracting historical climate data from WorldClim 2.1
#' @description extractWCdata returns a data frame based on specified climatic variables.
#' This function modifies the \href{https://github.com/rspatial/raster/blob/master/R/getData.R}{getData function from raster R package}, to extract world climate data of version 2.1 instead of version 1.4.
#' @param sites object of class "data.frame" with coordinates of sites from which to extract data.
#' @param long character. Name of column from \code{sites} with longitude.
#' @param lat character. Name of column from \code{sites} with latitude.
#' @param res numeric. Spatial resolution. Default 2.5.
#' @param var character. Climatic variable(s) to be extracted: 'tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'.
#' @return An object of class "data.frame" with specified climatic variables for coordinates in \code{sites}.
#' @details A grid can be created with any particular coordinates and used as input for \code{sites} (see section 'Examples'). \code{extractWCdata} will use the given coordinates to extract data from the WorldClim 2.1 database.
#' The extracted data will most likely contain NAs for sites where climate data is not available. These should be removed or imputed before using the data to make predictions.
#' @author Zakaria Kehel, Fawzy Nawar, Bancy Ngatia, Khadija Aouzal
#' @examples
#' if(interactive()){
#'  # Create grid
#'  sp1 <- seq(-16, 115, length = 10)
#'  sp2 <- seq(25, 59, length = 10) 
#'  sp <- expand.grid(x = sp1, y = sp2)
#'
#'  # Extract data using grid
#'  sp.df0 <- extractWCdata(sp, long = 'x', lat = 'y', var = 'tavg')
#'  sp.df <- na.omit(sp.df0)
#'  }
#' @rdname extractWCdata
#' @export
#' @importFrom sf st_as_sf
#' @importFrom raster extract


# Main function for extracting WorldClim data
extractWCdata <- function(sites, long, lat, var, res = 2.5){
  
  # Remove records with NA coordinates
  out <- list(
    is.na(sites[[long]]),
    is.na(sites[[lat]])
  )
  
  outReduced <- !Reduce("|", out)
  sites <- sites[outReduced,]
  
  # Convert to sf object
  sf_sites <- sf::st_as_sf(sites, coords = c(long, lat), crs = 4326)
  
  # Get rasters for each var and set naming schema
  for (ivar in var){
    rasterfile <- .getRasterData(var = ivar, res = res)
    for (i in 1:length(names(rasterfile))){
      f.name <- names(rasterfile)[i]
      var.name <- sub(paste(".*",res,"m_", sep = ''), "", f.name)
      message(var.name)
      
      # Extract raster values to sf object
      extracted_values <- raster::extract(rasterfile[[i]], sf::st_coordinates(sf_sites), method = 'simple')
      sites[ , var.name] <- extracted_values
    }
  }
  return(sites)
}

# Helper function for getting raster data
.getRasterData <- function(var, res) {
  stopifnot(var %in% c('tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'))
  
  # Set path
  path <- getwd()
  path <- paste0(path, "/WorldClim_", res, "/")
  dir.create(path, showWarnings = FALSE)
  
  # Construct URL naming schema
  theurl <- paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_", res, "m_", var, ".zip")
  zip <- paste0("wc2.1_", res, "m_", var, ".zip")
  zipfile <- paste0(path, zip)
  
  if (var != 'bio') {
    tiffiles <- paste0("wc2.1_", res, "m_", var, "_", sprintf("%02d", 1:12), ".tif")
  } else {
    tiffiles <- paste0("wc2.1_", res, "m_", var, "_", 1:19, ".tif")  
  }
  
  
  files <- paste0(path, tiffiles)
  fc <- sum(file.exists(files))
  
  # Download and unzip the file if not all files are present
  if (fc < length(files)) {
    if (!file.exists(zipfile)) {
      .download(theurl, zipfile)
      if (!file.exists(zipfile)) {
        message("Could not download file -- perhaps it does not exist.")
      }
    }
    utils::unzip(zipfile, exdir = dirname(zipfile))
  }
  
  # gather climate rasters
  st <- raster::stack(files)
  raster::projection(st) <- "+proj=longlat +datum=WGS84"
  return(st)
}


# Function to download a file from a URL and save it with a specified filename
.download <- function(url, filename, overwrite = FALSE) {
  
  # Generate a temporary filename with the '.download' suffix
  fn <- tempfile(fileext = '.download')
  
  # Initialize success flag
  success <- FALSE
  
  # Attempt to download the file from the URL to the temporary filename
  tryCatch({
    res <- utils::download.file(url = url, destfile = fn,
                                method = "auto", quiet = FALSE,
                                mode = "wb", cacheOK = TRUE, timeout=600)
    if (res == 0) {
      success <- TRUE
    }
  }, error = function(e) {
    success <- FALSE
    message("Download failed: ", e$message)
  })
  
  # Check if the download was successful
  if (success) {
    
    # Check if file already exists and if overwrite is not allowed
    if (file.exists(filename) && !overwrite) {
      stop("File already exists and overwrite is set to FALSE.")
    }
    
    # Use tryCatch to catch any file operation errors
    tryCatch({
      
      # Store the current warning level and set it back at the end
      w <- getOption('warn')
      on.exit(options('warn' = w), add = TRUE)
      
      # Try to rename the file; if it fails, try copying
      if (!file.rename(fn, filename)) { 
        if (!file.copy(fn, filename, overwrite = overwrite)) {
          stop("Failed to copy the file.")
        }
      }
      
      # Remove the temporary file if it still exists
      if (file.exists(fn)) {
        file.remove(fn)
      }
      
    }, error = function(e) {
      stop("An error occurred: ", e$message)
    })
    
  } else {
    stop("Could not download the file.")
  }
}

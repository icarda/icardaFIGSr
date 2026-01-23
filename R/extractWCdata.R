#' @title Extract Historical Climate Data from WorldClim 2.1
#' 
#' @description 
#' Extracts global climate data from the WorldClim 2.1 database for
#'  specific geographic locations provided in a data frame.
#' @param sites A \code{data.frame} containing the coordinates of the target locations.
#' @param long The name of the column in \code{sites} representing longitude.
#' @param lat The name of the column in \code{sites} representing latitude.
#' @param var Character vector. Climatic variable(s) to extract: \code{'tavg'},
#' \code{'tmin'}, \code{'tmax'}, \code{'prec'}, \code{'bio'}, \code{'srad'},
#' \code{'vapr'}, or \code{'wind'}.
#' @param res Numeric. Spatial resolution in minutes of a degree.
#' Options are 0.5, 2.5, 5, or 10. Default is 2.5.
#' @return A \code{data.frame} merging the original \code{sites} data
#'  with the extracted climatic variables.
#' @details The function uses the [geodata](https://cran.r-project.org/package=geodata)
#'  package to download WorldClim 2.1 data
#'  and [terra](https://cran.r-project.org/package=terra) for fast spatial extraction.
#' @author Zakaria Kehel, Fawzy Nawar, Bancy Ngatia, Khadija Aouzal, Chafik Analy, Tamara Ortiz
#' @examples
#' \dontrun{
#' # Define coordinates
#' my_sites <- data.frame(
#'   site_id = 1:3,
#'   lon = c(-16, 50, 115),
#'   lat = c(25, 40, 59)
#' )
#'
#' # Extract bioclimatic variables
#' clim_data <- extractWCdata(my_sites, long = "lon", lat = "lat", var = "bio")
#' }
#' @name extractWCdata
#' @importFrom terra vect rast extract
#' @importFrom geodata worldclim_global
#' @export

extractWCdata <- function(sites, long, lat, var, res = 2.5) {

  # Increase timeout for large downloads
  options(timeout = max(600, getOption("timeout")))
  
  # Basic validation
  if (!is.data.frame(sites) || !all(c(long, lat) %in% colnames(sites))) {
    stop("Sites must be a data frame with '", long, "' and '", lat, "' columns")
  }
  if (!all(var %in% c('tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'))) {
    stop("Invalid variable(s) specified. Choose from 'tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', or 'wind'.")
  }
  if (!res %in% c(0.5, 2.5, 5, 10)) {
    stop("Resolution must be one of: 0.5, 2.5, 5, or 10.")
  }

  # Convert to SpatVector once
  pts_spatial <- terra::vect(as.data.frame(sites), geom = c(long, lat), crs = "EPSG:4326")

  # Download and stack rasters
  clim_list <- lapply(var, function(v) {
    tryCatch(
      geodata::worldclim_global(var = v, res = res, path = getwd()),
      error = function(e) { 
        warning("Failed to download variable: ", v)
        return(NULL)
      }
    )
  })

  # Remove NULLs and combine into one multi-layer SpatRaster
  clim_list <- Filter(Negate(is.null), clim_list)
  if (length(clim_list) == 0) return(NULL)

  full_stack <- terra::rast(clim_list)

  # Rename layers
  names(full_stack) <- gsub("^wc2.1_.*?_", "", names(full_stack))

  # Extract values
  extracted_vals <- terra::extract(full_stack, pts_spatial, ID = FALSE)

  # Combine original and climate data
  final_data <- cbind(sites, extracted_vals)

  return(final_data)
}

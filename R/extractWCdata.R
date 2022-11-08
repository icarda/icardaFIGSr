#' @title Extracting World Climatic Data
#' @description extractWCdata returns a data frame based on specified climatic variables. 
#' @param sites object of class "data.frame" with coordinates of sites from which to extract data.
#' @param long character. Name of column from \code{sites} with longitude.
#' @param lat character. Name of column from \code{sites} with latitude.
#' @param res numeric. Spatial resolution. Default 2.5
#' @param var character. Climatic variable(s) to be extracted: 'tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'
#' @return An object of class "data.frame" with specified climatic variables for coordinates in \code{sites}.
#' @details A grid can be created with any particular coordinates and used as input for \code{sites} (see section 'Examples'). \code{extractWCdata} will use the given coordinates to extract data from the WorldClim2.1 database.
#' The extracted data will most likely contain NA's for sites where climatic data is not available. These should be removed or imputed before using the data to make predictions.
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
#' @importFrom sp SpatialPoints
#' @importFrom raster extract

extractWCdata <- function(sites, long, lat, var, res = 2.5){
  
  #remove records having NA coordinates
  out <- list(
    is.na(sites[[long]]),
    is.na(sites[[lat]])
  )

  outReduced <- !Reduce("|", out)
  sites <- sites[outReduced,]
  xy <- cbind(sites[[long]], sites[[lat]])
  sp <- sp::SpatialPoints(xy)
  
  for (ivar in var){
    
    rasterfile <- .getRasterData(var = ivar, res = res)
    
    for (i in 1:length(names(rasterfile))){
      
      f.name <- names(rasterfile)[i]
      var.name <- sub(paste(".*",res,"m_", sep = ''), "", f.name)
      message(var.name)
      sites[ , var.name] <- raster::extract(rasterfile[[i]], sp, method = 'simple')
      
    }
    
  }
  
  return(sites)
}


.getRasterData <- function(var, res){
  
  stopifnot(var %in% c('tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'))
  
  path <- getwd()
  path <- paste(path, '/WorldClim_', res, '/', sep='')
  dir.create(path, showWarnings=FALSE)
  
  theurl = paste("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_", res,"m_", var,".zip", sep='')
  
  zip <- paste('wc2.1_', res, 'm_', var ,'.zip', sep='')
  zipfile <- paste(path, zip, sep='')
  
  if (var  != 'bio') {
    tiffiles <- paste('wc2.1_', res, 'm_', var, '_', sprintf("%02d",1:12), '.tif', sep='')
  } else {
    tiffiles <- paste('wc2.1_', res, 'm_', var,'_', 1:19, '.tif', sep='')	
  }
  
  files <- paste(path, tiffiles, sep='')
  fc <- sum(file.exists(files))
  
  if ( fc < length(files) ) {
    if (!file.exists(zipfile)) {
        .download(theurl, zipfile)
        if (!file.exists(zipfile))	{ 
          message("\n Could not download file -- perhaps it does not exist") 
        }
    }	
    utils::unzip(zipfile, exdir=dirname(zipfile))
  }
  
  st <- raster::stack(files)

  raster::projection(st) <- "+proj=longlat +datum=WGS84"
  return(st)
}


.download <- function(url, filename) {
  fn <- paste(tempfile(), '.download', sep='')
  res <- utils::download.file(url=url, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
  if (res == 0) {
    w <- getOption('warn')
    on.exit(options('warn' = w))
    suppressWarnings()
    if (! file.rename(fn, filename) ) { 
      file.copy(fn, filename)
      file.remove(fn)
    }
  } else {
    stop('could not download the file' )
  }
}
#' @title Extracting Daily Climatic Variables Based on Onset of Planting
#' @description this function Extracts Daily values of climatic variables from remote ICARDA data based on Onset of Planting, it returns a list based on specified climatic variables. Each variable will have 365 values for each day of the (onset) year beginning with planting day.
#' @param sites character. Names of sites from which to extract data.
#' @param crop character. Crop code in ICARDA database. See section 'Details' for a list of crops.
#' @param var character. Climatic variable(s) to be extracted.
#' @param cv boolean. If \code{TRUE}, returns a data frame with coefficient of variation for each variable for each day of the onset year. Default: FALSE.
#' @return An object of class "data.frame" with specified climatic variables for names in \code{sites}.
#'
#' If \code{cv = TRUE}, the object is a list containing three data frames: the first one with average daily values of climatic variables, the second one with daily coefficient of variation for each climatic variable, and the third one with phenotypic variables and number of day in calendar year when each occurs at the sites specified in \code{sites}.
#'
#' If \code{cv = FALSE}, the object is a list containing two data frames: the first one with average daily values of climatic variables, and the second one with phenotypic variables and number of day in calendar year when each occurs at the sites specified in \code{sites}.
#' @details Similar to \code{\link[icardaFIGSr]{getDaily}} except the extracted data is based on 365 days starting from the onset of planting.
#' Crops available in ICARDA's genebank documentation system include the following:
#' \describe{
#'  \item{'ICAG'}{Aegilops}
#'  \item{'ICB'}{Barley}
#'  \item{'ICBW'}{Bread wheat}
#'  \item{'ILC'}{Chickpea}
#'  \item{'ICDW'}{Durum wheat}
#'  \item{'ILB'}{Faba bean}
#'  \item{'BPL'}{Faba bean BPL}
#'  \item{'IFMI'}{Forage and range}
#'  \item{'IFLA'}{Lathyrus}
#'  \item{'ILL'}{Lentil}
#'  \item{'IFMA'}{Medicago annual}
#'  \item{'IC'}{Not mandate cereals}
#'  \item{'IFPI'}{Pisum}
#'  \item{'ICPW'}{Primitive wheat}
#'  \item{'IFTR'}{Trifolium}
#'  \item{'IFVI'}{Vicia}
#'  \item{'ICWH'}{Wheat hybrids}
#'  \item{'ICWW'}{Wheat wild relatives}
#'  \item{'ILWC'}{Wild Cicer}
#'  \item{'ICWB'}{Wild Hordeum}
#'  \item{'ILWL'}{Wild Lens}
#'  \item{'ICWT'}{Wild Triticum}
#' }
#'
#'  Alternatively, the list of available crops can be fetched from ICARDA's online server using \code{\link[icardaFIGSr]{getCrops}}.
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Bancy Ngatia  
#' @examples
#' \dontrun{
#'  # Extract onset data for durum wheat
#'  durum <- getAccessions(crop = 'Durum wheat', coor = FALSE)
#'  onset <- getOnset(sites = unique(durum$SiteCode), crop = 'ICDW',
#'                    var = c('tavg', 'prec'), cv = TRUE)
#'
#'  # Get data frame with climatic variables from list object returned
#'  onset.clim <- onset[[1]]
#'
#'  # Get data frame with coefficient of variation from list object
#'  # returned (when cv = TRUE)
#'  onset.cv <- onset[[2]] 
#'
#'  # Get data frame with phenotypic variables from list object returned
#'  onset.pheno <- onset[[3]]
#'  }
#' @seealso
#'  \code{\link[reshape2]{dcast}},
#'  \code{\link[icardaFIGSr]{getCrops}}
#' @name getOnset
#' @importFrom reshape2 dcast
#' @export

getOnset <- function(sites, crop, var, cv = FALSE) {
  
  # Increase timeout for large data
  options(timeout = max(700, getOption("timeout")))
  
  # Validate input arguments
  if (!is.character(sites)) {
    stop("Sites must be a single or multiple character string")
  }
  
  crop_codes <- c("ICAG", "ICB", "ICBW", "ILC", "ICDW", "ILB", "BPL", "IFMI",
                  "IFLA", "ILL", "IFMA", "IC", "IFPI", "ICPW", "IFTR", "IFVI",
                  "ICWH", "ICWW", "ILWC", "ICWB", "ILWL", "ICWT")
  
  if (!is.character(crop) || length(crop) != 1 || !crop %in% crop_codes) {
    stop("Crop must be a single character string among codes returned by getCrops().")
  }
  
  if (!is.character(var)) {
    stop("Variable must be a single or multiple character string: tavg, prec, rh")
  }
  
  if (!is.logical(cv) || length(cv) != 1) {
    stop("cv should be TRUE or FALSE")
  }
  
  message("Data loading started ....")
  load(url("https://grs.icarda.org/FIGS/IcardaFigsData.RData"))
  message("Loading completed!")
  
  if (crop == 'ICDW'){
    onsetfile = ICDW_Onset_Phen
  }else if (crop == 'ICB'){
    onsetfile = ICB_Onset_Phen
  }else if (crop == 'ICBW'){
    onsetfile = ICBW_Onset_Phen
  }else if (crop == 'ILL'){
    onsetfile = ILL_Onset_Phen
  }else if (crop == 'ILB'){
    onsetfile = ILB_Onset_Phen
  }else if (crop == 'ILC'){
    onsetfile = ILC_Onset_Phen
  }else if (crop == 'IFPI'){
    onsetfile = IFPI_Onset_Phen
  }
  
  onsetfile <- droplevels(subset(onsetfile, onsetfile$onset_figs < 365))
  climate.df1 <- droplevels(subset(climate.df, climate.df$site_code %in% sites))
  climate.cv.df1 <- droplevels(subset(climate.cv.df, climate.cv.df$site_code %in% sites))
  
  loclevels <- levels(onsetfile$site_code)
  
  onset.climate.df <- droplevels(subset(climate.df1, climate.df1$site_code %in% loclevels))
  onset.climate.cv.df <- droplevels(subset(climate.cv.df1, climate.cv.df1$site_code %in% loclevels))
  
  onset.climate.df <- droplevels(merge(onset.climate.df, onsetfile, by = 'site_code', all.x = TRUE))
  onset.climate.cv.df <- droplevels(merge(onset.climate.cv.df, onsetfile, by = 'site_code', all.x = TRUE))
  
  onset.climate.df$Day <- as.numeric(onset.climate.df$Day)
  onset.climate.cv.df$Day <- as.numeric(onset.climate.cv.df$Day)
  
  onset.climate.df <- onset.climate.df[order(onset.climate.df$site_code, onset.climate.df$Day), ]
  onset.climate.cv.df <- onset.climate.cv.df[order(onset.climate.cv.df$site_code, onset.climate.cv.df$Day), ]
  
  onset.climate.df$DAP <- ifelse(onset.climate.df$Day <  onset.climate.df$onset_figs, onset.climate.df$Day - onset.climate.df$onset_figs + 366, onset.climate.df$Day - onset.climate.df$onset_figs + 1)
  onset.climate.cv.df$DAP <- ifelse(onset.climate.cv.df$Day <  onset.climate.cv.df$onset_figs, onset.climate.cv.df$Day - onset.climate.cv.df$onset_figs + 366, onset.climate.cv.df$Day - onset.climate.cv.df$onset_figs + 1)
  
  onset.climate.df <- onset.climate.df[order(onset.climate.df$site_code, onset.climate.df$DAP), ]
  onset.climate.cv.df <- onset.climate.cv.df[order(onset.climate.cv.df$site_code, onset.climate.cv.df$DAP), ]
  
  var0 <- c('site_code', 'DAP', var)
  cv0 <- paste0(var, '.cv', sep = '')
  cv1 <- cv0[which(cv0 %in% colnames(onset.climate.cv.df))]
  names.cv <- colnames(climate.cv.df)
  
  if(cv) {
    onset.climate.df <- onset.climate.df[ , var0]
    droplevels(onset.climate.df)
    onsetData <- NULL
    onsetData$site_code <- levels(as.factor(onset.climate.df$site_code))
    for (i in 3:length(var0)){
      onset.climate.df1 <- onset.climate.df[ , c(1, 2, i)]
      tmp <- reshape2::dcast(onset.climate.df1, site_code ~ DAP, mean)
      tmp <- tmp[ , 1:366]
      colnames(tmp)[2:366] <- paste(var0[i], seq(from = 1, to = 365, by = 1), sep = '')
      tmp$site_code <- NULL
      onsetData <- cbind(onsetData, tmp)
    }
    if(any(cv1 %in% names.cv)) {
      var1 <- c('site_code', 'DAP', cv1)
      onset.climate.cv.df <- onset.climate.cv.df[ , var1]
      droplevels(onset.climate.cv.df)
      onsetCVData <- NULL
      onsetCVData$site_code <- levels(as.factor(onset.climate.cv.df$site_code))
      for (i in 3:length(var1)){
        onset.climate.cv.df1 <- onset.climate.cv.df[ , c(1, 2, i)]
        tmpCV <- reshape2::dcast(onset.climate.cv.df1, site_code ~ DAP, mean)
        tmpCV <- tmpCV[ , 1:366]
        colnames(tmpCV)[2:366] <- paste(var1[i], seq(from = 1, to = 365, by = 1), sep = '')
        tmpCV$site_code <- NULL
        onsetCVData <- cbind(onsetCVData, tmpCV)
      }
      
      result <- list()
      result[[1]] <- onsetData
      result[[2]] <- onsetCVData
      result[[3]] <- onsetfile
      return(result)
    }
  }
  
  else if(!cv) {
    var1 <- c('site_code', 'DAP', var)
    onset.climate.df <- onset.climate.df[ , var1]
    droplevels(onset.climate.df)
    onsetData <- NULL
    onsetData$site_code <- levels(as.factor(onset.climate.df$site_code))
    for (i in 3:length(var1)){
      onset.climate.df1 <- onset.climate.df[ , c(1, 2, i)]
      
      tmp <- reshape2::dcast(onset.climate.df1, site_code ~ DAP, mean)
      tmp <- tmp[ , 1:366]
      colnames(tmp)[2:366] <- paste(var1[i], seq(from = 1, to = 365, by = 1), sep = '')
      tmp$site_code <- NULL
      onsetData <- cbind(onsetData, tmp)
    }
    
    result <- list()
    result[[1]] <- onsetData
    result[[2]] <- onsetfile
    return(result)
  }
}

#' @title Extracting Daily Climatic Variables
#' @description this function extracts daily values of climatic variables from ICARDA Data, it returns a list or data frame based on specified climatic variables. Each variable will have 365 values for each day of the calendar year.
#' @param sites character. Names of sites from which to extract data.
#' @param var character. Climatic variable(s) to be extracted.
#' @param cv boolean. If \code{TRUE}, returns a data frame with coefficient of variation for each variable for each day of the calendar year. Default: FALSE.
#' @return An object with specified climatic variables for names in \code{sites}.
#'
#' If \code{cv = TRUE}, the object is a list containing two data frames: the first one with average daily values of climatic variables, and the second one with daily coefficient of variation for each climatic variable.
#'
#' If \code{cv = FALSE}, the object is a data frame with average daily values of climatic variables.
#' @details ICARDA data has to be accessible either from a local directory on the computer or from an online repository. \code{getDaily} will extract the climatic variables specified in \code{var} for the sites specified in \code{sites}.
#'
#' For daily data, the function extracts average daily values starting from the first day of the calendar year, i.e. January 1, until the last day of the calendar year, i.e. December 31. Thus, 365 columns with daily values are created for each variable.
#' @author Zakaria Kehel, Bancy Ngatia
#' @examples
#' if(interactive()){
#'  # Extract daily data for durum wheat
#'  durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
#'  daily <- getDaily(sites = levels(as.factor(durum$SiteCode)),
#'                    var = c('tavg', 'prec', 'rh'), cv = TRUE)
#'
#'  # Get data frame with coefficient of variation from list object
#'  # returned (when cv = TRUE)
#'  daily.cv <- daily[[2]]
#'  }
#' @seealso
#'  \code{\link[reshape2]{cast}}
#' @rdname getDaily
#' @export
#' @importFrom reshape2 dcast
#' @importFrom utils capture.output

getDaily <- function(sites, var, cv = FALSE) {
  
  message("Data loading started ....")
  load(url("https://grs.icarda.org/FIGS/IcardaFigsData.RData"))
  message("Loading completed!")

  daily.climate.df <- droplevels(subset(climate.df, climate.df$site_code %in% sites))
  daily.climate.cv.df <- droplevels(subset(climate.cv.df, climate.cv.df$site_code %in% sites))
  daily.climate.df <- daily.climate.df[order(daily.climate.df$site_code), ]
  daily.climate.cv.df <- daily.climate.cv.df[order(daily.climate.cv.df$site_code), ]

  var0 <- c('site_code', 'Day', var)
  cv0 <- paste0(var, '.cv', sep = '')
  cv1 <- cv0[which(cv0 %in% colnames(daily.climate.cv.df))]
  names.cv <- colnames(climate.cv.df)

  if(cv) {
    daily.climate.df <- daily.climate.df[ , var0]
    droplevels(daily.climate.df)
    dailyData <- NULL
    dailyData$site_code <- levels(as.factor(daily.climate.df$site_code))
    for (i in 3:length(var0)){
      daily.climate.df1 <- daily.climate.df[ , c(1, 2, i)]
      tmp <- reshape2::dcast(daily.climate.df1, site_code ~ Day, mean)
      tmp <- tmp[ , 1:366]
      colnames(tmp)[2:366] <- paste(var0[i], seq(from = 1, to = 365, by = 1), sep = '')
      tmp$site_code <- NULL
      dailyData <- cbind(dailyData, tmp)
    }
    if(any(cv1 %in% names.cv)) {
      var1 <- c('site_code', 'Day', cv1)
      daily.climate.cv.df <- daily.climate.cv.df[ , var1]
      droplevels(daily.climate.cv.df)
      dailyCVData <- NULL
      dailyCVData$site_code <- levels(as.factor(daily.climate.cv.df$site_code))
      for (i in 3:length(var1)){
        daily.climate.cv.df1 <- daily.climate.cv.df[ , c(1, 2, i)]
        tmpCV <- reshape2::dcast(daily.climate.cv.df1, site_code ~ Day, mean)
        tmpCV <- tmpCV[ , 1:366]
        colnames(tmpCV)[2:366] <- paste(var1[i], seq(from = 1, to = 365, by = 1), sep = '')
        tmpCV$site_code <- NULL
        dailyCVData <- cbind(dailyCVData, tmpCV)
      }

      result <- list()
      result[[1]] <- dailyData
      result[[2]] <- dailyCVData
      return(result)
    }
  }

  else if(!cv) {
    var1 <- c('site_code', 'Day', var)
    daily.climate.df <- daily.climate.df[ , var1]
    droplevels(daily.climate.df)
    dailyData <- NULL
    dailyData$site_code <- levels(as.factor(daily.climate.df$site_code))
    for (i in 3:length(var1)){
      daily.climate.df1 <- daily.climate.df[ , c(1, 2, i)]
      tmp <- reshape2::dcast(daily.climate.df1, site_code ~ Day, mean)
      tmp <- tmp[ , 1:366]
      colnames(tmp)[2:366] <- paste(var1[i], seq(from = 1, to = 365, by = 1), sep = '')
      tmp$site_code <- NULL
      dailyData <- cbind(dailyData, tmp)
    }
    return(dailyData)
  }
}

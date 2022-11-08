#' @title durumWC
#' @description 200 sites from durum wheat collection and their world clim data.
#' @docType data
#' @usage data(durumWC)
#' @format The data includes the site unique identifier, longitude, latitude and 55 worldclim data \href{https://www.worldclim.org}{worldclim}
#' 
#' @examples
#' if(interactive()){
#'  # Load durum wheat data with world climatic variables obtained from WorldClim database
#'  data(durumWC)
#'  }
"durumWC"


#' @title durumDaily
#' @description 200 sites from durum wheat collection and their daily climatic data.
#' @docType data
#' @usage data(durumDaily)
#' @format The data includes the site unique identifier and daily data for 4 climatic variables (tmin, tmax, precipitation and relative humidity)
#' 
#' @examples
#' if(interactive()){
#'  # Load durum wheat data with their daily climatic variables obtained from ICARDA database
#'  data(durumDaily)
#' }
"durumDaily"


#' @title septoriaDurumWC
#' @description A sample data including daily data for 4 climatic variables (tmin, tmax, precipitation and relative humidity) and evaluation for Septoria Tritici
#' @docType data
#' @usage data(septoriaDurumWC)
#' @format 200 sites from durum wheat collection and their daily climatic data and evaluation for Septoria Tritici.
#' 
#' @examples
#' if(interactive()){
#'  #Load durum wheat data with septoria scores and climatic variables obtained from WorldClim database
#'  data(septoriaDurumWC)
#' }
"septoriaDurumWC"

#' @title FIGS subset for wheat sodicity resistance
#'
#' @description FIGS subset for wheat sodicity resistance
#' constructed using the harmonized world soil database HWSD
#'
#' @docType data
#'
#' @usage data(FIGS)
#'
#' @format A data frame with 201 rows and 15 variables
#'
#'
#' @references 
#' \href{http://www.fao.org/3/aq361e/aq361e.pdf}{HWSD}
#'
#'
#' @examples
#' if(interactive()){
#'  data(FIGS)
#' }

"FIGS"

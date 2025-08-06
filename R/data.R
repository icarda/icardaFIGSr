#' @title BarleyRNOWC
#' @name BarleyRNOWC
#' @description 200 samples with kernel row number observations from barley collection and 55 corresponding worldclim data.
#' @docType data
#' @usage data("BarleyRNOWC")
#' @format More details about the climate data can be found here \href{https://www.worldclim.org/data/worldclim21.html}{worldclim}
#' 
#' @examples
#' if(interactive()){
#'  # Load barley Kernel Row Number data with world climatic variables obtained from WorldClim database
#'  data("BarleyRNOWC")
#'  }
"BarleyRNOWC"

#' @title DurumWheatDHEWC
#' @name DurumWheatDHEWC
#' @description 193 samples of Days to heading from durum wheat collection and 55 worldclim data.
#' @docType data
#' @usage data("DurumWheatDHEWC")
#' @format More details about the climate data can be found here \href{https://www.worldclim.org/data/worldclim21.html}{worldclim}
#' 
#' @examples
#' if(interactive()){
#'  # Load durum wheat Days to Heading data with world climatic variables obtained from WorldClim database
#'  data("DurumWheatDHEWC")
#'  }
"DurumWheatDHEWC"

#' @title durumWC
#' @name durumWC
#' @description 200 sites from durum wheat collection and their world clim data.
#' @docType data
#' @usage data("durumWC")
#' @format The data includes the site unique identifier, longitude, latitude and 55 worldclim data \href{https://www.worldclim.org/data/worldclim21.html}{worldclim}
#' 
#' @examples
#' if(interactive()){
#'  # Load durum wheat data with world climatic variables obtained from WorldClim database
#'  data("durumWC")
#'  }
"durumWC"

#' @title durumDaily
#' @name durumDaily
#' @description 200 sites from durum wheat collection and their daily climatic data.
#' @docType data
#' @usage data("durumDaily")
#' @format The data includes the site unique identifier and daily data for 4 climatic variables (tmin, tmax, precipitation and relative humidity)
#' 
#' @examples
#' if(interactive()){
#'  # Load durum wheat data with their daily climatic variables obtained from ICARDA database
#'  data("durumDaily")
#' }
"durumDaily"

#' @title septoriaDurumWC
#' @name septoriaDurumWC
#' @description A sample data including monthly data for 3 climatic variables (tmin, tmax and precipitation), 19 bioclimatic variables, and evaluation for Septoria Tritici
#' @docType data
#' @usage data("septoriaDurumWC")
#' @format 200 sites from durum wheat collection and their monthly climatic data and evaluation for Septoria Tritici.
#' 
#' @examples
#' if(interactive()){
#'  #Load durum wheat data with septoria scores and climatic variables obtained from WorldClim database
#'  data("septoriaDurumWC")
#' }
"septoriaDurumWC"

#' @title FIGS
#' @name FIGS
#' @description FIGS subset for wheat sodicity resistance
#' constructed using the FAO harmonized world soil database HWSD
#' @docType data
#' @usage data("FIGS")
#' @format A data frame with 201 rows and 15 variables
#'
#' @references 
#' \href{https://openknowledge.fao.org/server/api/core/bitstreams/2f45beea-e165-4264-951e-20b7c1da65c0/content}{HWSD}
#'
#' @examples
#' if(interactive()){
#'  data("FIGS")
#' }
"FIGS"
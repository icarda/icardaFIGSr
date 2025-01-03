#' @title Getting Traits Associated with Crops from the ICARDA's Genebank Documentation System
#' @description Return a data frame containing traits associated with a particular crop, their description and related identifiers.
#' @param crop character. Crop for which to get available traits.
#' @return A data frame with traits that are associated with the crop specified in \code{crop}.
#' @details \code{getCropTraits} returns a data frame of traits together with their IDs and coding system used for each trait.
#'
#' Possible inputs for \code{crop} include:
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
#'  A list of available crops to use as input for \code{crop} can also be obtained from ICARDA's online server using \code{\link[icardaFIGSr]{getCrops}}.
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#'  # Get traits for bread wheat
#'  breadTraits <- getCropTraits(crop = 'Bread wheat')
#' }
#' @name getCropTraits
#' @importFrom httr handle POST content
#' @export

getCropTraits <- function(crop) {
  
  if (missing(crop)) {
    print("Please specify a crop from the list below:")
    return(getCrops())
  } else {
    
    
    if (!(".credentials" %in% ls(envir = .icardaFIGSEnv, all.names = TRUE))) {
      .authenticate()
    }
    
    credentials <- get(".credentials", envir = .icardaFIGSEnv)
    
    username <- credentials$username
    password <- credentials$password
    handle <- httr::handle("https://grs.icarda.org/web_services/getTraits.php")
    
    body <- list(
      user = username
      ,pass  = password
      ,crop = crop
    )
    
    response <- httr::POST(handle = handle, body = body)
    result <- httr::content(response, type = "text/csv")
    
    pattern = "invalid"
    
    if(grepl(pattern, response, ignore.case = TRUE)){
      rm(.credentials, envir = .icardaFIGSEnv)
    }
    
    return(result)
  }
}

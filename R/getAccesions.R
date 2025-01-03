#' @title Getting Accession Data from ICARDA's Genebank Documentation System
#' @description Return a data frame with accession data for the specified crop.
#' @param crop character. Crop for which to get accession data. See section 'Details' for available crops or use \code{\link[icardaFIGSr]{getCrops}} function. Default: "".
#' @param ori string. Country of origin using the ISO 3166-1 alpha-3 country codes. Default: NULL.
#' @param IG integer. Unique identifier of accession. Default: "".
#' @param doi boolean. If \code{TRUE} , the function will return the digital object identifiers DOI for the accessions. Default: FALSE.
#' @param taxon boolean. If \code{TRUE}, the function will return the taxon information of the accessions. Default: FALSE.
#' @param collectionYear boolean. If \code{TRUE}, the function will return the year of the collecting mission. Default: FALSE.
#' @param coor boolean. If \code{TRUE}, returns only georeferenced accessions containing longitude and latitude. Default: FALSE.
#' @param available boolean. If \code{TRUE}, returns the availability of accessions for distribution, Default: FALSE.
#' @return A data frame with accession passport data for specified crop in \code{crop} from the locations in \code{ori}.
#' @details Types of crops available include:
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
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#'  # Obtain accession data for durum wheat
#'  durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
#'  }
#' @name getAccessions
#' @importFrom httr handle POST content
#' @export

getAccessions <- function(crop = "", ori = NULL, IG = "", doi = FALSE, taxon = FALSE, collectionYear = FALSE,  coor = FALSE, available = FALSE) {
  
  query = ""
  if(!missing(crop)) {
    query <- paste("CROP_NAME = '", crop ,"'",  sep = "")
  }
  
  
  if(!is.null(ori)) {
    ori = paste(ori, collapse = "','")
    if (query == "") {
      query <- paste(query, "ORI IN ('", ori, "')" , sep = "")
    } else {
      query <- paste(query, " AND ORI IN ('", ori, "')" , sep = "")
    }
  }
  
  
  if(!missing(IG)) {
    IG = paste(IG, collapse = ',')
    if (query == "") {
      query <- paste(query, "IG IN (", IG , ")" , sep = "")
    } else {
      query <- paste(query, " AND IG IN (", IG , ")" , sep = "")
    }
  }
  
  res = "error"
  if(query != "") {
    
    if (!(".credentials" %in% ls(envir = .icardaFIGSEnv, all.names = TRUE))) {
      .authenticate()
    }
    
    credentials <- get(".credentials", envir = .icardaFIGSEnv)
    
    username <- credentials$username
    password <- credentials$password
    
    handle <- httr::handle("https://grs.icarda.org/web_services/accessionsToR.php")
    
    body <- list(
      user = username
      ,pass  = password
      ,crop = crop
      ,DataFilter = query
      ,coor = coor
      ,doi = doi
      ,taxon = taxon
      ,col_year = collectionYear
      ,available = available
    )
    
    response <- httr::POST(handle = handle, body = body)
    res <- httr::content(response, type = "text/csv")
    pattern = "invalid"
    
    if(grepl(pattern, response, ignore.case = TRUE)){
      rm(.credentials, envir = .icardaFIGSEnv)
    }
    
  }
  return(res)
}


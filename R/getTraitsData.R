#' @title Getting Trait Values of Accessions for a Specific Trait
#' @description Return a data frame with observed values of accessions for associated Trait
#' @param IG factor. Unique identifier of accession.
#' @param traitID integer. Unique identifier of trait (from \code{\link[icardaFIGSr]{getCropTraits}}).
#' @return A data frame with scores for the trait specified in \code{traitID} for the accessions given in \code{IG}.
#' @details Possible inputs for \code{traitID} can be found using the \code{\link[icardaFIGSr]{getCropTraits}} function (see section 'Examples').
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#'  # Check trait ID for septoria and get septoria data for durum wheat
#'  durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
#'  durumTraits <- getCropTraits(crop = 'Durum wheat')
#'  septoria <- getTraitsData(IG = durum$IG, traitID = 145)
#'  }
#' @name getTraitsData
#' @importFrom httr handle POST content
#' @export

getTraitsData <- function(IG, traitID) {
  
  IG = paste(IG, collapse = ',')
  if(traitID == "") {
    print("Error: Please provide a valid traitID")
    result = NULL
  } else {
    
    if (!(".credentials" %in% ls(envir = .icardaFIGSEnv, all.names = TRUE))) {
      .authenticate()
    }
    
    credentials <- get(".credentials", envir = .icardaFIGSEnv)
    
    username <- credentials$username
    password <- credentials$password
    handle <- httr::handle("https://grs.icarda.org/web_services/getTraitsData.php")
    
    body <- list(
      user = username
      ,pass  = password
      ,traitID = traitID
      ,IGs = IG
    )
    
    response <- httr::POST(handle = handle, body = body)
    result <- httr::content(response, type = "text/csv", col_types = "nnncnnnnnnnc")
    pattern = "invalid"
    
    if(grepl(pattern, response, ignore.case = TRUE)){
      rm(.credentials, envir = .icardaFIGSEnv)
    }
  }
  return(result)
}
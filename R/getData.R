.icardaFIGSEnv <- new.env(parent = emptyenv())

# GUI for getting username and password from user

.authenticate <- function(){
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "Login")
  
  ss <- "Please enter username and password"
  tcltk::tkgrid(tcltk::tklabel(tt, text = ss), columnspan = 2, padx = 50, pady = 10)
  
  usr <- tcltk::tclVar("")
  pwd <- tcltk::tclVar("")
  
  usr_label <- tcltk::tklabel(tt, text = "Username:")
  pwd_label <- tcltk::tklabel(tt, text = "Password:")
  
  usr_input <- tcltk::tkentry(tt, width = "30", textvariable = usr)
  pwd_input <- tcltk::tkentry(tt, width = "30", textvariable = pwd, show = "*")
  
  tcltk::tkgrid(usr_label, usr_input, sticky = "ew", padx = 5)
  tcltk::tkgrid(pwd_label, pwd_input, sticky = "ew", padx = 5)
  
  on_okay <- function() {
    .credentials <- list("username" = tcltk::tclvalue(usr), "password" = tcltk::tclvalue(pwd))
    assign(".credentials", .credentials, envir = .icardaFIGSEnv)
    tcltk::tkdestroy(tt)
  }
  
  ok_button <- tcltk::tkbutton(tt, text = " OK ", command = on_okay)
  tcltk::tkbind(pwd_input, "<Return>", on_okay)
  tcltk::tkgrid(ok_button, columnspan = 2, pady = 5)
  
  tcltk::tkfocus(tt)
  tcltk::tkwait.window(tt)
  
}

#' @title Retrieving ICARDA Genebank Accession Passport Data.
#' @description Get ICARDA Genebank passport data by crop or accession numbers.
#' @param crop Crop name. Default: "".
#' @param ori Country of origin (ISO 3166-1 alpha-3 code). Default: NULL.
#' @param IG List of accession numbers. Default: "".
#' @param doi \text{[Deprecated]} No longer used as of version 2.0.0.
#' @param taxon \text{[Deprecated]} No longer used as of version 2.0.0.
#' @param collectionYear \text{[Deprecated]} No longer used as of version 2.0.0.
#' @param coor If \code{TRUE}, returns only georeferenced accessions. Default: FALSE.
#' @param available If \code{TRUE}, returns only available accessions for distribution, Default: FALSE.
#' @param other_id If \code{TRUE}, returns other IDs associated with accessions. Default: FALSE.
#' @return Data frame of accession passport data by crop or accession numbers.
#' @details Available crops can be retrieved using \code{\link[icardaFIGSr]{getCrops}}.
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#'  # Obtain accession data for durum wheat
#'  durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
#'  }
#' @name getAccessions
#' @importFrom httr handle POST content
#' @export

getAccessions <- function(crop = "",
                          ori = NULL,
                          IG = "",
                          doi = lifecycle::deprecated(),
                          taxon = lifecycle::deprecated(),
                          collectionYear = lifecycle::deprecated(),
                          coor = FALSE,
                          available = FALSE,
                          other_id = FALSE) {

  if (lifecycle::is_present(doi)) {
    lifecycle::deprecate_warn("2.0.0", "getAccessions(doi)", details = "doi is no longer used.")
  }

  if (lifecycle::is_present(taxon)) {
    lifecycle::deprecate_warn("2.0.0", "getAccessions(taxon)", details = "taxon is no longer used.")
  }
  
  if (lifecycle::is_present(collectionYear)) {
    lifecycle::deprecate_warn("2.0.0", "getAccessions(collectionYear)", details = "collectionYear is no longer used.")
  }

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
      ,available = available
      ,other_id = other_id
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

#' @title Getting Traits Associated with Crops from the ICARDA's Genebank Database
#' @description Return a data frame containing traits associated with a particular crop, their description and related identifiers.
#' @param crop character. Crop for which to get available traits.
#' @return A data frame with traits that are associated with the crop specified in \code{crop}.
#' @details \code{getTraits} returns a data frame of traits together with their IDs and descriptions.
#' The list of available crops to use as input for \code{crop} can be fetched from ICARDA's Genebank database using \code{\link[icardaFIGSr]{getCrops}}.
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Get traits for bread wheat
#'  breadTraits <- getTraits(crop = 'Bread wheat')
#'  }
#' }
#' @rdname getTraits
#' @export
#' @importFrom httr handle POST content

getTraits <- function(crop) {
  
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


#' @title Getting Trait Values of Accessions for a Specific Trait
#' @description Return a data frame with observed values of accessions for associated Trait
#' @param IG factor. Unique identifier of accession.
#' @param traitID integer. Unique identifier of trait (from \code{\link[icardaFIGSr]{getTraits}}).
#' @return A data frame with scores for the trait specified in \code{traitID} for the accessions given in \code{IG}.
#' @details Possible inputs for \code{traitID} can be found using the \code{\link[icardaFIGSr]{getTraits}} function (see section 'Examples').
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Check trait ID for septoria and get septoria data for durum wheat
#'  durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
#'  durumTraits <- getTraits(crop = 'Durum wheat')
#'  septoria <- getTraitsData(IG = durum$IG, traitID = 145)
#'  }
#' }
#' @rdname getTraitsData
#' @export
#' @importFrom httr handle POST content

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
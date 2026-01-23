.icardaFIGSEnv <- new.env(parent = emptyenv())

#' @title Internal function to authenticate user via GUI
#' @description creates a GUI to prompt the user for their username and password

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

#' @title Internal function to get resource data from ICARDA Genebank API
#' @description This internal function handles authentication and data retrieval from ICARDA Genebank API.
#' @return Parsed JSON response from the API.
#' @param url The URL for getting the resource data
#' @param body body for POST requests
#' @importFrom httr2 oauth_client req_oauth_password request req_body_json req_perform resp_body_json

.get_data <- function(url, body = NULL) {

  # Set up OAuth client
  client <- httr2::oauth_client(
    id        = "7ac1999e52ff54d84a2fc8ca018544e0",
    token_url = "https://grs.icarda.org/api/v1/token",
    auth      = "body",
    name      = "icardaFIGSr"
  )

  # Retrieve the credentials list from the internal environment
  if (!(".credentials" %in% ls(envir = .icardaFIGSEnv, all.names = TRUE))) {
    .authenticate()
  }

  creds <- get(".credentials", envir = .icardaFIGSEnv)

  req <- httr2::request(url)

  if (!is.null(body)) {
    req <- req %>% httr2::req_body_json(body)
  }

  req <- req %>%
    httr2::req_oauth_password(
      client     = client,
      username   = creds$username,
      password   = creds$password,
      scope      = "read"
    )

  resp <- tryCatch({
    httr2::req_perform(req) %>% httr2::resp_body_json()
    #req_dry_run(req)
  }, error = function(e) {
    
    if (grepl("401", e$message) || grepl("invalid_grant", e$message)) {
      message("Authentication failed: Session expired or wrong credentials. Please log in.")

      # Trigger the UI
      .authenticate()

      # Retry with new credentials
      httr2::request(url) %>%
        httr2::req_body_json(body) %>%
        httr2::req_oauth_password(
          client   = client,
          username = creds$username,
          password = creds$password,
          scope   = "read"
        ) %>%
        httr2::req_perform()
    } else {
      stop(e)
    }
  })

  return(resp)
}

#' @title Getting List of Crops Maintained in ICARDA Genebank
#' @description Retrieve a list of available crops in ICARDA's Genebank.
#' @return A data frame with crop codes and names.
#' @author Khadija Aouzal, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#'  # Get list of available crops
#'  crops <- getCrops()
#'  }
#' @name getCrops
#' @importFrom purrr pluck
#' @importFrom dplyr bind_rows
#' @export

getCrops <- function() {

  json_data <- .get_data("https://grs.icarda.org/api/v1/crops")

  crops <- json_data %>%
    purrr::pluck("data") %>%
    dplyr::bind_rows()

  return(crops)

}


#' @title Getting Accession Passport Data by Crop or Accession Numbers
#' @description Return a data frame containing passport data of accessions filtered by crop name or accession numbers.
#' @param crop Crop name.
#' @param ori Country of origin (ISO 3166-1 alpha-3 code).
#' @param IG List of accession numbers.
#' @param doi \code{[Deprecated]} No longer used as of version 2.0.0.
#' @param taxon \code{[Deprecated]} No longer used as of version 2.0.0.
#' @param collectionYear \code{[Deprecated]} No longer used as of version 2.0.0.
#' @param coor If \code{TRUE}, returns only georeferenced accessions.
#' @param available If \code{TRUE}, returns only available accessions for distribution.
#' @param other_id If \code{TRUE}, returns other IDs associated with accessions.
#' @return Data frame of accession passport data by crop or accession numbers.
#' @details Available crops can be retrieved using \code{\link[icardaFIGSr]{getCrops}}.
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#'  # Obtain accession data for durum wheat
#'  durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
#'  }
#' @name getAccessions
#' @importFrom purrr pluck
#' @importFrom dplyr bind_rows
#' @importFrom lifecycle is_present deprecate_warn
#' @export

getAccessions <- function(crop = "",
                          ori = NULL,
                          IG = NULL,
                          doi = lifecycle::deprecated(),
                          taxon = lifecycle::deprecated(),
                          collectionYear = lifecycle::deprecated(),
                          coor = FALSE,
                          available = FALSE,
                          other_id = FALSE) {

  dep_args <- list(doi = doi, taxon = taxon, collectionYear = collectionYear)
  for (arg_name in names(dep_args)) {
    if (lifecycle::is_present(dep_args[[arg_name]])) {
      lifecycle::deprecate_warn("2.0.0", sprintf("getAccessions(%s)", arg_name))
    }
  }
  
  if(crop == "" && is.null(IG)){
    stop("Please provide at least one filtering criterion (crop or IG).")
  }
  
  crop <- if(crop == "") NULL else list(crop)
  
  fields <- list()
  if(other_id) fields <- list("other_id")
  
  body_list <- list(
    filters = list(
      CropName = crop,
      AccessionNumber = IG,
      CountryOfOrigin = ori,
      available = available,
      georef = coor),
    fields = fields)
  
  json_data <- .get_data("https://grs.icarda.org/api/v1/accessions", body_list)
  
  passport_data <- json_data %>%
    purrr::pluck("data") %>%
    dplyr::bind_rows()
  
  return(passport_data)
}

#' @title Getting Traits Descriptors for a Specific Crop
#' @description Return a data frame with traits descriptors associated with a specific crop.
#' @param crop Crop name.
#' @return A data frame with traits descriptors associated with the specified crop.
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
#' @importFrom purrr pluck
#' @importFrom dplyr bind_rows

getTraits <- function(crop) {

  if (missing(crop) || crop == "") {    
    stop("Please specify a valid crop name")
  } else {
    body_list <- list(crops = list(crop))
    json_data <- .get_data("https://grs.icarda.org/api/v1/traits", body_list)
    crop_traits <- json_data %>%
      purrr::pluck("data") %>%
      dplyr::bind_rows()
    return(crop_traits)
  }
}


#' @title Getting Trait Data for Given Accessions
#' @description Return a data frame with scores for a specific trait for given accessions.
#' @param IG Accession numbers.
#' @param traitID Unique identifier of trait (from \code{\link[icardaFIGSr]{getTraits}}).
#' @return A data frame with scores for the trait specified in \code{traitID} for the accessions given in \code{IG}.
#' @details Valid inputs for \code{traitID} can be fetched using the \code{\link[icardaFIGSr]{getTraits}} function.
#' @author Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Check trait ID for septoria and get septoria data for durum wheat
#'  durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
#'  durumTraits <- getTraits(crop = 'Durum wheat')
#'  septoria <- getTraitsData(IG = durum$AccessionNumber, traitID = 145)
#'  }
#' }
#' @rdname getTraitsData
#' @export
#' @importFrom purrr pluck
#' @importFrom dplyr bind_rows

getTraitsData <- function(IG, traitID) {

  body_list <- list(
    traitID = traitID,
    IGs = IG
    )

  json_data <- .get_data("https://grs.icarda.org/api/v1/traitdata", body_list)

  trait_data <- json_data %>%
    purrr::pluck("data") %>%
    dplyr::bind_rows()

  return(trait_data)
}
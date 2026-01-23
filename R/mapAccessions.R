#' @title Plotting Accessions on Map
#' @description Map accessions according to their collecting sites.
#' @param df object of class "data.frame" with coordinates of accessions and target variable.
#' @param long character. Column name from \code{df} representing longitude.
#' @param lat character. Column name from \code{df} representing latitude.
#' @param y Default: NULL, column name from \code{df} representing the target variable.
#' @return A world map with plotted points showing collecting sites.
#'
#' @author Khadija Aouzal, Zakaria Kehel
#' @examples
#' \dontrun{
#'  # Loading FIGS subset for wheat sodicity resistance
#'  data(FIGS)
#'  # World Map showing locations of accessions
#'  mapAccessions(df = FIGS, long = "Longitude", lat = "Latitude")
#'  
#'  # Map plotting locations of accessions with points coloured 
#'  # based on a gradient scale of SodicityIndex values
#'  mapAccessions(FIGS, long = "Longitude", lat = "Latitude", 
#'                y = "SodicityIndex")
#'  # Map plotting locations of accessions with points
#'  # coloured based on levels of y 
#'  mapAccessions(FIGS, long = "Longitude", lat = "Latitude", 
#'  y = "PopulationType")
#'  }
#' @name mapAccessions
#' @import leaflet 
#' @export


mapAccessions <- function(df, long, lat, y = NULL){
  
  if(is.null(y)) {
    leaflet::leaflet() %>% 
      leaflet::addProviderTiles('Esri.WorldGrayCanvas')  %>%
      leaflet::addCircleMarkers(data = df, lng = df[[long]], lat = df[[lat]],
                                color = "#2d7436",
                                radius = 1.5,
                                fill = TRUE,
                                fillColor = "#2d7436",
                                fillOpacity = 0.2, weight = 2) 
  } else {
    ## omit NAs in y column
    df.na.omit <- df[!is.na(df[[y]]), ]
    
    if (is.numeric(df[[y]])){
      pal <- leaflet::colorNumeric(
        palette = c("#2d7436", "#ED7506"),
        domain = df[[y]],
        na.color = "#808080"
      )
    } else {
      pal <- leaflet::colorFactor(
        palette = 'Dark2',
        domain = df[[y]],
        na.color = "#808080"
      )
    }
    
    leaflet::leaflet() %>% 
      leaflet::addProviderTiles('Esri.WorldGrayCanvas')  %>%
      leaflet::addCircleMarkers(data = df.na.omit, lng = df.na.omit[[long]], lat = df.na.omit[[lat]],
                                color = ~pal(df.na.omit[[y]]),
                                radius = 1.5,
                                fill = TRUE,
                                fillColor = ~pal(df.na.omit[[y]]),
                                label = ~df.na.omit[[y]],
                                fillOpacity = 1, weight = 2, group = "without NAs") %>%
      leaflet::addCircleMarkers(data = df, lng = df[[long]], lat = df[[lat]],
                                color = ~pal(df[[y]]),
                                radius = 1.5,
                                fill = TRUE,
                                fillColor = ~pal(df[[y]]),
                                label = ~df[[y]],
                                fillOpacity = 1, weight = 2, group = "with NAs") %>%
      leaflet::addLegend("bottomright", pal = pal, values = df[[y]], opacity = 1,  title = y) %>%
      leaflet::addLayersControl(baseGroups = c("with NAs","without NAs"),
                       options = leaflet::layersControlOptions(collapsed = FALSE))
    
  }
}

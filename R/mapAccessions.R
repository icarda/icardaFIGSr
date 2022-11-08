#' @title Plotting Accessions on Map. 
#' @description this function returns a map with points showing where accessions are located.
#' @param df object of class "data.frame" with coordinates of accessions and target variable.
#' @param long character. Column name from \code{df} representing longitude.
#' @param lat character. Column name from \code{df} representing latitude.
#' @param y Default: NULL, column name from \code{df} representing the target variable.
#' @return A world map with plotted points showing locations of accessions.
#'
#' @author Khadija Aouzal, Zakaria Kehel
#' @examples
#' if(interactive()){
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
#' @rdname mapAccessions
#' @export
#' @import leaflet 



mapAccessions <- function(df, long, lat, y = NULL){
  
  if(is.null(y)) {
    leaflet::leaflet() %>% leaflet::addTiles() %>% 
      leaflet::addProviderTiles('Esri.WorldTopoMap')  %>%
      leaflet::addCircleMarkers(data = df, lng = df[[long]], lat = df[[lat]],
                                color = "#2d7436",
                                radius = 5,
                                fill = TRUE,
                                fillColor = "#2d7436",
                                fillOpacity = 0.2, weight = 2) 
  }
  else {
    ## omit NAs in y column
    df.na.omit <- df[!is.na(df[[y]]), ] 
    
    if (is.numeric(df[[y]])){
      pal <- leaflet::colorNumeric(
        palette = c("#2d7436", "#ED7506"),
        domain = df[[y]],
        na.color = "#808080"
      )
    }
    else {
      pal <- leaflet::colorFactor(
        palette = c("#2d7436", "#ED7506"),
        domain = df[[y]],
        na.color = "#808080"
      )
    }
    
    leaflet::leaflet() %>% leaflet::addTiles() %>% 
      leaflet::addProviderTiles('Esri.WorldTopoMap')  %>%
      leaflet::addCircleMarkers(data = df.na.omit, lng = df.na.omit[[long]], lat = df.na.omit[[lat]],
                                color = ~pal(df.na.omit[[y]]),
                                radius = 5,
                                fill = TRUE,
                                fillColor = ~pal(df.na.omit[[y]]),
                                label = ~df.na.omit[[y]],
                                fillOpacity = 0.2, weight = 2, group = "withoutNAs") %>%
      leaflet::addCircleMarkers(data = df, lng = df[[long]], lat = df[[lat]],
                                color = ~pal(df[[y]]),
                                radius = 5,
                                fill = TRUE,
                                fillColor = ~pal(df[[y]]),
                                label = ~df[[y]],
                                fillOpacity = 0.2, weight = 2, group = "withNAs") %>%
      leaflet::addLegend("bottomright", pal = pal, values = df[[y]], opacity = 1,  title = y) %>%
      addLayersControl(baseGroups = c("withNAs","withoutNAs"),
                       options = layersControlOptions(collapsed = FALSE))
    
  }
}

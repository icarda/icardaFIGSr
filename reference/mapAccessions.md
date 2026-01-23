# Plotting Accessions on Map

Map accessions according to their collecting sites.

## Usage

``` r
mapAccessions(df, long, lat, y = NULL)
```

## Arguments

- df:

  object of class "data.frame" with coordinates of accessions and target
  variable.

- long:

  character. Column name from `df` representing longitude.

- lat:

  character. Column name from `df` representing latitude.

- y:

  Default: NULL, column name from `df` representing the target variable.

## Value

A world map with plotted points showing collecting sites.

## Author

Khadija Aouzal, Zakaria Kehel

## Examples

``` r
if (FALSE) { # \dontrun{
 # Loading FIGS subset for wheat sodicity resistance
 data(FIGS)
 # World Map showing locations of accessions
 mapAccessions(df = FIGS, long = "Longitude", lat = "Latitude")
 
 # Map plotting locations of accessions with points coloured 
 # based on a gradient scale of SodicityIndex values
 mapAccessions(FIGS, long = "Longitude", lat = "Latitude", 
               y = "SodicityIndex")
 # Map plotting locations of accessions with points
 # coloured based on levels of y 
 mapAccessions(FIGS, long = "Longitude", lat = "Latitude", 
 y = "PopulationType")
 } # }
```

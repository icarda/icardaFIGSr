# Extracting Daily Climatic Variables Based on Onset of Planting

Extracts daily values of climatic variables from ICARDA data based on
Onset of Planting. It returns a list based on specified climatic
variables. Each variable will have 365 values for each day of the
(onset) year beginning with planting day.

## Usage

``` r
getOnset(sites, crop, var, cv = FALSE)
```

## Arguments

- sites:

  Names of sites from which to extract data.

- crop:

  Crop code in ICARDA Genebank database. See section 'Details' for a
  list of crops.

- var:

  Climatic variable(s) to be extracted. choices : tavg, prec, rh

- cv:

  If `TRUE`, returns a data frame with coefficient of variation for each
  variable for each day of the onset year. Default: FALSE.

## Value

If `cv = TRUE`, returns a list containing three data frames: the first
one with average daily values of climatic variables, the second one with
phenological variables and number of day in calendar year when each
occurs at the sites specified in `sites`, and the third one with daily
coefficient of variation for each climatic variable. If `cv = FALSE`,
returns a list containing two data frames: the first one with average
daily values of climatic variables, and the second one with phenological
variables and number of day in calendar year when each occurs at the
sites specified in `sites`.

## Details

Similar to [`getDaily`](getDaily.md) except the extracted data is based
on 365 days starting from the onset of planting.

## See also

[`dcast`](https://rdrr.io/pkg/reshape2/man/cast.html),
[`getCrops`](getCrops.md)

## Author

Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Bancy Ngatia

## Examples

``` r
if (FALSE) { # \dontrun{
 # Extract onset data for durum wheat
 durum <- getAccessions(crop = 'Durum wheat', coor = FALSE)
 onset <- getOnset(sites = unique(durum$SiteCode), crop = 'ICDW',
                   var = c('tavg', 'prec'), cv = TRUE)

 # Get data frame with climatic variables from list object returned
 onset.clim <- onset[[1]]

 # Get data frame with coefficient of variation from list object
 # returned (when cv = TRUE)
 onset.cv <- onset[[2]]

 # Get data frame with phenological variables from list object returned
 onset.pheno <- onset[[3]]
 } # }
```

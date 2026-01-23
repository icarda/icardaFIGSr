# Extracting Daily Climatic Variables

Extracts daily values of climatic variables.

## Usage

``` r
getDaily(sites, var, cv = FALSE)
```

## Arguments

- sites:

  character vector. Names of sites from which to extract data.

- var:

  character vector. Climatic variable(s) to be extracted.

- cv:

  boolean. If `TRUE`, returns a data frame with coefficient of variation
  for each variable for each day of the calendar year. Default: FALSE.

## Value

An object with specified climatic variables for specified site code in
`sites`.

If `cv = TRUE`, the object is a list containing two data frames: the
first one with average daily values of climatic variables, and the
second one with daily coefficient of variation for each climatic
variable.

If `cv = FALSE`, the object is a data frame with average daily values of
climatic variables.

## Details

`getDaily` extracts the daily climatic variables specified in `var` for
the sites specified in `sites` from an online repository. The function
then extracts average daily values starting from the first day of the
calendar year, until the last day of the calendar year.

## See also

[`cast`](https://rdrr.io/pkg/reshape2/man/cast.html)

## Author

Zakaria Kehel, Bancy Ngatia

## Examples

``` r
if (FALSE) { # \dontrun{
 # Extract daily data for durum wheat
 durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
 daily <- getDaily(sites = levels(as.factor(durum$SiteCode)),
                   var = c('tavg', 'prec', 'rh'), cv = TRUE)

 # Get data frame with coefficient of variation from list object
 # returned (when cv = TRUE)
 daily.cv <- daily[[2]]
 } # }
 
```

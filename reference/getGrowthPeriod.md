# Calculating Growing Degree Days and Lengths of Growth Stages for Various Crops Using Onset Data from ICARDA's Genebank Database

Calculates growing degree days (GDD) as well as cumulative GDD, and
returns a list of various data frames based on specified arguments.

## Usage

``` r
getGrowthPeriod(sitecode, crop, base, max, gdd = FALSE)
```

## Arguments

- sitecode:

  Vector with names of sites from which to extract onset data.

- crop:

  Crop name. Calculations are available for 'Barley', 'Bread wheat',
  'Chickpea', 'Durum wheat' and 'Lentil'.

- base:

  Minimum temperature constraint for the crop.

- max:

  Maximum temperature constraint for the crop.

- gdd:

  If `TRUE`, returns a data frame containing calculated GDD and
  accumulated GDD together with climatic variables used for the
  calculations. Default: FALSE.

## Value

A list object with different data frames depending on specified option
in `gdd`. If `gdd = TRUE`, the object is a list containing three data
frames: the first one with lengths of different growing stages, the
second one with original onset data with phenological variables, and the
third one with calculated GDD and accumulated GDD for the sites
specified in `sitecode`. If `gdd = FALSE`, the object is a list
containing two data frames: the first one with lengths of different
growing stages, and the second one with original onset data with
phenological variables for the sites specified in `sitecode`.

## Details

Growing degree days for various crops are calculated using average daily
minimum and maximum temperature values obtained from onset data. The
temperature constraints specified in `base` and `max` are first applied
before the calculations are done. These constraints ensure very low or
high temperatures which prevent growth of a particular crop are not
included. Crops for which GDD calculations are available include:
'Barley', 'Bread wheat', 'Chickpea', 'Durum wheat' and 'Lentil'. Each of
these can be supplied as options for the argument `crop`. Cumulative GDD
values determine the length of different growing stages. Growing stages
vary depending on the type of crop. Durum wheat, bread wheat and barley
have five growth stages, i.e. beginning of heading, beginning and
completion of flowering, and beginning and completion of grain filling.
Chickpea and lentil have four growth stages, i.e. beginning of
flowering, completion of 50% flowering, beginning of seed filling, and
completion of 90% maturity (chickpea) or of full maturity (lentil). The
length of the full growth cycle of the crop for each site is also given
in the output data frame.

## Author

Chafik Analy, Khadija Aouzal, Zakaria Kehel, Bancy Ngatia

## Examples

``` r
if (FALSE) { # \dontrun{
 # Calculate GDD for durum wheat
 data(durumDaily)

 growth <- getGrowthPeriod(sitecode = durumDaily$site_code,
                           crop = 'Durum wheat', base = 0,
                           max = 35, gdd = TRUE)

 # Get the dataframe with lengths of growth stages from the returned list
 growth.lengths <- growth[[1]]

 # Get the dataframe with phenological variables from the returned list
 growth.pheno <- growth[[2]]

 # Get the dataframe with GDD, cumulative GDD,
 # tmin and tmax from the returned list (when gdd = TRUE)
 growth.gdd <- growth[[3]]
 } # }
 
```

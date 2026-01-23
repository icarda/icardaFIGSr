# Extracting Sites Climate Data

## getDaily()

[`getDaily()`](../reference/getDaily.md) retrieves daily climate Data
from ICARDA Genebank database given a vector of `site_codes` and a
vector of `climate variables` among possible choices. This example
demonstrates how to use the function to retrieve average daily
temperature (tavg) for a subset of sites.

``` r
library(icardaFIGSr)
# Extract daily data for durum wheat
durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
daily <- getDaily(sites = levels(as.factor(durum$SiteCode)), 
                    var = c('tavg', 'prec', 'rh'), cv = TRUE)
```

## extractWcdata()

[`extractWCdata()`](../reference/extractWCdata.md) extracts monthly
historical climate data from WorldClim v2.1 for the specified climate
variable(s) and sites.

``` r
library(icardaFIGSr)

# Get barley accessions
barley_accs <- getAccessions(crop = "Barley", coor = TRUE)

barley_accs_bio <- extractWCdata(barley_accs, long = 'Longitude', lat = 'Latitude', 
                                  var = 'bio', res = 2.5)
```

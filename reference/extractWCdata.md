# Extract Historical Climate Data from WorldClim 2.1

Extracts global climate data from the WorldClim 2.1 database for
specific geographic locations provided in a data frame.

## Usage

``` r
extractWCdata(sites, long, lat, var, res = 2.5)
```

## Arguments

- sites:

  A `data.frame` containing the coordinates of the target locations.

- long:

  The name of the column in `sites` representing longitude.

- lat:

  The name of the column in `sites` representing latitude.

- var:

  Character vector. Climatic variable(s) to extract: `'tavg'`, `'tmin'`,
  `'tmax'`, `'prec'`, `'bio'`, `'srad'`, `'vapr'`, or `'wind'`.

- res:

  Numeric. Spatial resolution in minutes of a degree. Options are 0.5,
  2.5, 5, or 10. Default is 2.5.

## Value

A `data.frame` merging the original `sites` data with the extracted
climatic variables.

## Details

The function uses the
[geodata](https://cran.r-project.org/package=geodata) package to
download WorldClim 2.1 data and
[terra](https://cran.r-project.org/package=terra) for fast spatial
extraction.

## Author

Zakaria Kehel, Fawzy Nawar, Bancy Ngatia, Khadija Aouzal, Chafik Analy,
Tamara Ortiz

## Examples

``` r
if (FALSE) { # \dontrun{
# Define coordinates
my_sites <- data.frame(
  site_id = 1:3,
  lon = c(-16, 50, 115),
  lat = c(25, 40, 59)
)

# Extract bioclimatic variables
clim_data <- extractWCdata(my_sites, long = "lon", lat = "lat", var = "bio")
} # }
```

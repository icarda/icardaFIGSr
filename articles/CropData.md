# Accessing Crop-Related Data

This vignette lists the functions from the icardaFIGSr package used for
retrieving crop-related data from ICARDA Genebank database, and provides
examples of fetching key data.

> Running some functions will prompt you to enter your user name and
> password, if not previously authentified.

## Functions

### getCrops()

[`getCrops()`](../reference/getCrops.md) retrieves a list of crop names
and their corresponding codes.

### getAccessions()

[`getAccessions()`](../reference/getAccessions.md) returns the passport
data of accessions for a specified crop or a list of accession numbers.
The retrieved passport descriptors include:

- **IG:** Accession number, the unique identifier for accessions within
  a genebank.
- **Crop:** Crop name associated with the accession.
- **Taxon name:** Scientific name of the sample composed of
  genus+species.
- **Country:** 3-letter ISO 3166-1 code of the sample’s country of
  origin.
- **PopulationType:** Biological status of the sample.
- **SiteCode:** Code identifying the specific site of collection.
- **Longitude:** Longitude of the collecting site in decimal degrees
  format.
- **Latitude:** Latitude of the collecting site in decimal degrees
  format.
- **Altitude:** Altitude of the collecting site.
- **Province:** Name of administrative division below the country level.
- **ADM1:** First level administrative division in the country.
- **SITE:** Locality where the sample was collected.
- **CollectionYear:** Year when the sample was collected.

### getTraits()

[`getTraits()`](../reference/getTraits.md) returns a dataframe for each
specified crop describing the available traits ontologies. The dataframe
includes:

- **Id:** A unique identifier for each trait associated with the crop.
- **Trait:** The trait name being measured or observed (e.g., ‘Days to
  heading’).
- **Options:** Code-value pairs for coded traits (e.g., ‘1-Narrow;
  2-Medium; 3-Large’ for *flag leaf width* trait).
- **Field:** The abbreviated name of the trait.

### getTraitsData()

[`getTraitsData()`](../reference/getTraitsData.md) returns the observed
values for a specified trait and a list of accession numbers.

### getOnset()

[`getOnset()`](../reference/getOnset.md) extracts daily values of
climatic variables based on Onset of Planting for sites available in the
ICARDA Genebank database. Each variable will have 365 values for each
day of the (onset) year beginning with planting day.

### getGrowthPeriod()

[`getGrowthPeriod()`](../reference/getGrowthPeriod.md) calculates
growing degree days (GDD), cumulative GDD, and the length of growth
stages for various crops using onset data from
[`getOnset()`](../reference/getOnset.md) function.

## Examples

### Passport Data

``` r
# Load icardaFIGSr package
library(icardaFIGSr)

# List all crops maintained in the ICARDA Genebank
getCrops()

# Get all the cultivated barley accessions and their basic passport data
getAccessions(crop = 'barley')

# Include DOI, taxonomy, collection year and other identifiers to the output 
getAccessions(crop = 'barley', doi = TRUE, taxon = TRUE, collectionYear = TRUE, other_id = TRUE)

# Get only available accessions for distribution for barley
getAccessions(crop = 'barley', available = TRUE)

# Get only georefenced accessions for barley
getAccessions(crop = 'barley', coor = TRUE)

# Get accessions for barley and for which countries of origin are Morocco and Lebanon
getAccessions(crop = 'barley', ori = c('MAR','LBN'))

# Get the passport data of a list of accession numbers, i.e. IGs
# read the list of IGs from a csv file with a column holding IGs named e.g. IG
list_IGs <- read.csv('/path/to/csv/file.csv')
getAccessions(IG = list_IGs$IG)
```

``` r
library('icardaFIGSr')

# Load FIGS subset for wheat sodicity resistance
data(FIGS)

# World Map showing collecting sites of accessions
mapAccessions(df = FIGS, long = 'Longitude', lat = 'Latitude')
```

``` r

# Plotting collecting sites of accessions with points coloured based on a gradient scale of SodicityIndex values
mapAccessions(FIGS, long = 'Longitude', lat = 'Latitude', y = 'SodicityIndex')
```

``` r

# Plotting collecting sites of accessions with points coloured based on levels of y 
mapAccessions(FIGS, long = 'Longitude', lat = 'Latitude', y = 'PopulationType')
```

### Traits Data

``` r
# To get data of one trait for a list of accessions:
# Get the list of accessions for a crop of interest, e.g. barley:
barley_accs <- getAccessions(crop = 'barley')

# Get traits descriptors for barley
getTraits(crop = 'barley')

# Pick the traitID of the trait of interest from the output of getTraits (e.g. Awn roughness has traitID=83 for barley), and then get the trait observations
getTraitsData(IG = barley_accs$IG, traitID = 83)

# To get data for many traits at once, e.g. Awn roughness (traitID=83), Plant height (traitID=93) and Days to heading (traitID=89)
# Define the traits of interest
traits <- c(83,93,89)
# Initialize an empty list to store the results
Barley_Accessions_Traits <- list()

# Loop through each TraitID
for (trait_id in 1:length(traits)){
  # Extract trait data for the current TraitID
  trait_data <- getTraitsData(IG = barley_accs$IG,
                                traitID = traits[trait_id])

  if(nrow(trait_data)<1){
    next
  }

  # Check if the trait_data contains 'IG', 'YEAR' and a trait-specific 
  if (all(c("IG", "YEAR") %in% colnames(trait_data))){
    # Extract 'IG', 'YEAR', and the last column (trait-specific column)
    last_col <- colnames(trait_data)[ncol(trait_data)]
    trait_data <- trait_data[ ,c("IG","YEAR", last_col)]
    
    # Add Trait_name column
    trait_data$Trait_name <- last_col
    
    # Rename trait_value
    colnames(trait_data) <- c("IG","Year","Trait_value","Trait_name")
  }
  # Store the result in the list
  Barley_Accessions_Traits[[as.character(trait_id)]] <- trait_data
}

# Bind all datasets together (converting a list into a dataframe)
Barley_Accessions_Traits <- do.call(rbind.data.frame,
                                    Barley_Accessions_Traits)
```

### Onset Data

`getOnset` returns a list of 2 main datasets. The first dataset contains
the site’s code as the first column and 365 other columns each
corresponding to the day of the year of specified climate variables
starting from the onset date. When `cv=TRUE`, a third dataset is added
to the final output which contains the `coefficient of variation` of the
variable of interest.  
In the example below, we use site codes from durumWC dataset, specify
ICDW (durum wheat) as crop code (use
[`getCrops()`](../reference/getCrops.md) function to list available
cropCodes for your crop of interest), and specify `c('tavg','prec')` as
the climate variables. Please refer to the documentation using
[`?getOnset`](../reference/getOnset.md) for more details.  

``` r
library(icardaFIGSr)
# Load durumWC dataset
data("durumWC")
durumWC <- durumWC |> dplyr::ungroup()

# Get site codes
Sites <- levels(as.factor(durumDaily$site_code))

# Get onset data
onset <- getOnset(sites = Sites[1:2], crop = 'ICDW',
                     var = c('tavg','prec'), cv = TRUE)

# Get the dataframe with climatic variables from the list object 
onset.clim <- onset[[1]]

# Get the dataframe with phenological variables from the list object
onset.pheno <- onset[[2]]

# Get the dataframe with coefficient of variation from the list object
onset.climcv <- onset[[3]]
```

### Growth Period Data

[`getGrowthPeriod()`](../reference/getGrowthPeriod.md) returns a list of
three dataframes:

1.  Growth_Period: contains `sitecode`, `onset`, lengths of
    `growth stages` (expressed in days), and `Cycle` which is the sum of
    the growth stages.  
    The growth stages are named following abbreviation codes for each
    crop. Below is a glossary of used growth stage codes and their
    corresponding names.

| Stage Code | Stage Name             |
|:-----------|:-----------------------|
| HB         | Heading Begins         |
| FB         | Flowering Begins       |
| FC         | Flowering Complete     |
| FC50       | 50% Flowering Complete |
| GFB        | Grain Filling Begins   |
| GFC        | Grain Filling Complete |
| SF         | Seed Filling           |
| M          | Maturity               |

2.  Onset_Data: contains `sitecode`, `onset` and `growth stages` of all
    sites available for the specified crop.

3.  Growing_Degree_Days: includes `sitecode`, their temperature ranges
    `tmin` and `tmax`, the calculated growing degree days `gdd`, their
    cumulatives `cumgdd` and corresponding `days after planting`.

``` r
library(icardaFIGSr)
data(durumDaily)

# Get site codes
Sites <- levels(as.factor(durumDaily$site_code))

# Get growth period for durum wheat with specified temperature range
growth <- icardaFIGSr::getGrowthPeriod(sitecode = Sites,
                              crop = 'Durum wheat', base = 0,
                              max = 35, gdd = TRUE)

# Get the dataframe with lengths of growth stages from the returned list
growth.lengths <- growth[[1]]

# Get the dataframe with phenological variables from the returned list
growth.pheno <- growth[[2]]

# Get dataframe with GDD, cumulative GDD, tmin and tmax from the returned list (when gdd = TRUE)
growth.gdd <- growth[[3]]
```

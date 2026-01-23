# Getting Accession Passport Data by Crop or Accession Numbers

Return a data frame containing passport data of accessions filtered by
crop name or accession numbers.

## Usage

``` r
getAccessions(
  crop = "",
  ori = NULL,
  IG = NULL,
  doi = lifecycle::deprecated(),
  taxon = lifecycle::deprecated(),
  collectionYear = lifecycle::deprecated(),
  coor = FALSE,
  available = FALSE,
  other_id = FALSE
)
```

## Arguments

- crop:

  Crop name.

- ori:

  Country of origin (ISO 3166-1 alpha-3 code).

- IG:

  List of accession numbers.

- doi:

  `[Deprecated]` No longer used as of version 2.0.0.

- taxon:

  `[Deprecated]` No longer used as of version 2.0.0.

- collectionYear:

  `[Deprecated]` No longer used as of version 2.0.0.

- coor:

  If `TRUE`, returns only georeferenced accessions.

- available:

  If `TRUE`, returns only available accessions for distribution.

- other_id:

  If `TRUE`, returns other IDs associated with accessions.

## Value

Data frame of accession passport data by crop or accession numbers.

## Details

Available crops can be retrieved using [`getCrops`](getCrops.md).

## Author

Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar

## Examples

``` r
if (FALSE) { # \dontrun{
 # Obtain accession data for durum wheat
 durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
 } # }
```

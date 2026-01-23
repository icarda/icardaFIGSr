# Getting Trait Data for Given Accessions

Return a data frame with scores for a specific trait for given
accessions.

## Usage

``` r
getTraitsData(IG, traitID)
```

## Arguments

- IG:

  Accession numbers.

- traitID:

  Unique identifier of trait (from [`getTraits`](getTraits.md)).

## Value

A data frame with scores for the trait specified in `traitID` for the
accessions given in `IG`.

## Details

Valid inputs for `traitID` can be fetched using the
[`getTraits`](getTraits.md) function.

## Author

Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 # Check trait ID for septoria and get septoria data for durum wheat
 durum <- getAccessions(crop = 'Durum wheat', coor = TRUE)
 durumTraits <- getTraits(crop = 'Durum wheat')
 septoria <- getTraitsData(IG = durum$AccessionNumber, traitID = 145)
 }
} # }
```

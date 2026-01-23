# Getting Traits Associated with Crops from the ICARDA's Genebank Documentation System

Return a data frame containing traits associated with a particular crop,
their description and related identifiers.

## Usage

``` r
getCropTraits(crop)
```

## Arguments

- crop:

  character. Crop for which to get available traits.

## Value

A data frame with traits that are associated with the crop specified in
`crop`.

## Details

`getCropTraits` returns a data frame of traits together with their IDs
and coding system used for each trait.

Possible inputs for `crop` include:

- 'ICAG':

  Aegilops

- 'ICB':

  Barley

- 'ICBW':

  Bread wheat

- 'ILC':

  Chickpea

- 'ICDW':

  Durum wheat

- 'ILB':

  Faba bean

- 'BPL':

  Faba bean BPL

- 'IFMI':

  Forage and range

- 'IFLA':

  Lathyrus

- 'ILL':

  Lentil

- 'IFMA':

  Medicago annual

- 'IC':

  Not mandate cereals

- 'IFPI':

  Pisum

- 'ICPW':

  Primitive wheat

- 'IFTR':

  Trifolium

- 'IFVI':

  Vicia

- 'ICWH':

  Wheat hybrids

- 'ICWW':

  Wheat wild relatives

- 'ILWC':

  Wild Cicer

- 'ICWB':

  Wild Hordeum

- 'ILWL':

  Wild Lens

- 'ICWT':

  Wild Triticum

A list of available crops to use as input for `crop` can also be
obtained from ICARDA's online server using [`getCrops`](getCrops.md).

## Author

Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar, Chafik
Analy

## Examples

``` r
if (FALSE) { # \dontrun{
 # Get traits for bread wheat
 breadTraits <- getCropTraits(crop = 'Bread wheat')
} # }
```

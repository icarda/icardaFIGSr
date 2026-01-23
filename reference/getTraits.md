# Getting Traits Descriptors for a Specific Crop

Return a data frame with traits descriptors associated with a specific
crop.

## Usage

``` r
getTraits(crop)
```

## Arguments

- crop:

  Crop name.

## Value

A data frame with traits descriptors associated with the specified crop.

## Author

Khadija Aouzal, Amal Ibnelhobyb, Zakaria Kehel, Fawzy Nawar

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 # Get traits for bread wheat
 breadTraits <- getTraits(crop = 'Bread wheat')
 }
} # }
```

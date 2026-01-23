# Splitting Data

this function splits the Data into Train and Test Sets, it returns a
list containing two data frames for the train and test sets.

## Usage

``` r
splitData(data, seed = NULL, y, p, ...)
```

## Arguments

- data:

  object of class "data.frame" with target variable and predictor
  variables.

- seed:

  integer. Values for the random number generator. Default: NULL.

- y:

  character. Target variable.

- p:

  numeric. Proportion of data to be used for training.

- ...:

  additional arguments to be passed to `createDataPartition` function in
  `caret` package to control the way the data is split.

## Value

A list with two data frames: the first as train set, and the second as
test set.

## Details

`splitData` relies on the `createDataPartition` function from the
`caret` package to perform the data split.

If `y` is a factor, the sampling of observations for each set is done
within the levels of `y` such that the class distributions are more or
less balanced for each set.

If `y` is numeric, the data is split into groups based on percentiles
and the sampling done within these subgroups. See
[`createDataPartition`](https://rdrr.io/pkg/caret/man/createDataPartition.html)
for more details on additional arguments that can be passed.

## See also

[`createDataPartition`](https://rdrr.io/pkg/caret/man/createDataPartition.html)

## Author

Zakaria Kehel, Bancy Ngatia

## Examples

``` r
if (FALSE) { # \dontrun{
 # Split the data into 70/30 train and test sets for factor y
 data(septoriaDurumWC)
 split.data <- splitData(septoriaDurumWC, seed = 1234,
                         y = 'ST_S', p = 0.7)

 # Get training and test sets from list object returned
 trainset <- split.data$trainset
 testset <- split.data$testset
} # }
```

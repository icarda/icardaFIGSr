# Performance Measures for classification tasks

this function allows to obtain performance measures from Confusion
Matrix, it returns a data frame containing performance measures from the
confusion matrix given by the `caret` package.

## Usage

``` r
getMetrics(y, yhat, classtype)
```

## Arguments

- y:

  expression. The class variable.

- yhat:

  expression. The vector of predicted values.

- classtype:

  character or numeric. The number of levels in `y`.

## Value

Outputs an object with performance measures calculated from the
confusion matrix given by the `caret` package. A data frame is the
resulting output with the first column giving the name of the
performance measure, and the second column giving the corresponding
value.

## Details

`getMetrics` works with target variables that have two, three, four, six
or eight classes.

The function relies on the `caret` package to obtain the confusion
matrix from which performance measures are extracted. It can be run for
several algorithms, and the results combined into one data frame for
easier comparison (see section 'Examples').

Predictions have to be obtained beforehand and used as input for `yhat`.
The `predict.train` function in `caret` should be run without argument
`type` when obtaining the predictions.

## See also

[`confusionMatrix`](https://rdrr.io/pkg/caret/man/confusionMatrix.html)

## Author

Zakaria Kehel, Bancy Ngatia, Khadija Aziz, Chafik Analy

## Examples

``` r
if (FALSE) { # \dontrun{
# Obtain predictions from previous models
 
data(septoriaDurumWC)  
split.data <- splitData(septoriaDurumWC, seed = 1234, y = "ST_S", p = 0.7)
data.train <- split.data$trainset
data.test <- split.data$testset

knn.mod <- tuneTrain(data = septoriaDurumWC,y = 'ST_S',method = 'knn',positive = 'R')
nnet.mod <- tuneTrain(data = septoriaDurumWC,y = 'ST_S',method = 'nnet',positive = 'R') 

pred.knn <- predict(knn.mod$Model, newdata = data.test[ , -1])
pred.nnet <- predict(nnet.mod$Model, newdata = data.test[ , -1])

metrics.knn <- getMetrics(y = data.test$ST_S, yhat = pred.knn, classtype = 2)
metrics.nnet <- getMetrics(y = data.test$ST_S, yhat = pred.nnet, classtype = 2)
} # }
```

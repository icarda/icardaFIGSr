# Make Predictions

`make_prediction` makes predictions. It returns a list containing a data
frame of predictions or class probabilities, and corresponding plots.

## Usage

``` r
make_prediction(newdata, y, positive = NULL, model, scale = FALSE, auc = FALSE)
```

## Arguments

- newdata:

  A data frame. The test dataset.

- y:

  The name of the target variable.

- positive:

  The positive class for the target variable if `y` is a factor.
  Default: `NULL`.

- model:

  A model object. The trained model returned by
  [`caret::train`](https://rdrr.io/pkg/caret/man/train.html).

- scale:

  A logical value. If `TRUE`, scales the variable importance values to
  between 0-100. Default: `FALSE`.

- auc:

  A logical value. If `TRUE`, calculates the area under the ROC curve
  (AUC) for classification models. Default: `FALSE`.

## Value

A list containing predictions, class probabilities, and corresponding
plots.

- For classification models, class probabilities and ROC curves are
  included.

- For regression models, predictions and residuals vs. predicted plots
  are included.

## Details

This function uses `caret`, `ggplot2`, and `plotROC` for calculations
and plotting.

## See also

[`predict.train`](https://rdrr.io/pkg/caret/man/predict.train.html)

## Author

Zakaria Kehel, Bancy Ngatia, Khadija Aziz, Zainab Azough, Chafik Analy,
Khadija Aouzal

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example for classification model
  data("septoriaDurumWC")
  knn.mod <- caret::train(ST_S ~ ., data = septoriaDurumWC, method = "knn")
  testdata <- septoriaDurumWC
  knn.pred <- make_prediction(newdata = testdata, y = "ST_S", positive = "R", model = knn.mod)
  print(knn.pred)

  # Example with SVM and ROC curve
  svm.mod <- caret::train(ST_S ~ ., data = septoriaDurumWC, method = "svmLinear2",
   metric = "Accuracy")
  testdata <- septoriaDurumWC
  svm.pred <- make_prediction(newdata = testdata, y = "ST_S", positive = "R", model = svm.mod,
   auc = TRUE)
  print(svm.pred)
} # }
```

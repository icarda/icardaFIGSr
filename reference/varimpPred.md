# Variable Importance and Predictions

`varimpPred` calculates variable importance and makes predictions. It
returns a list containing a data frame of variable importance scores,
predictions or class probabilities, and corresponding plots.

## Usage

``` r
varimpPred(
  newdata,
  y,
  positive = NULL,
  model,
  scale = FALSE,
  auc = FALSE,
  predict = FALSE,
  type = "prob",
  ...
)
```

## Arguments

- newdata:

  A data frame. The test dataset.

- y:

  A character string. The name of the target variable.

- positive:

  A character string. The positive class for the target variable if `y`
  is a factor. Default: `NULL`.

- model:

  A model object. The trained model returned by
  [`caret::train`](https://rdrr.io/pkg/caret/man/train.html).

- scale:

  A logical value. If `TRUE`, scales the variable importance values to
  between 0-100. Default: `FALSE`.

- auc:

  A logical value. If `TRUE`, calculates the area under the ROC curve
  (AUC) for classification models. Default: `FALSE`.

- predict:

  A logical value. If `TRUE`, calculates predictions or class
  probabilities. Default: `FALSE`.

- type:

  A character string. The type of prediction, e.g., `"prob"` or `"raw"`.
  Default: `"prob"`.

- ...:

  Additional arguments to be passed to
  [`caret::varImp`](https://rdrr.io/pkg/caret/man/varImp.html).

## Value

A list containing variable importance scores, predictions, class
probabilities, and corresponding plots.

- For classification models, class probabilities and ROC curves are
  included if `predict = TRUE`.

- For regression models, predictions and residuals vs. predicted plots
  are included if `predict = TRUE`.

## Details

This function uses `caret`, `ggplot2`, and `plotROC` for calculations
and plotting. Variable importance is calculated based on the type of
model.

For example, for regression models, the absolute value of the
t-statistic of each parameter is used in the importance calculation. For
classification models, a variable importance score is calculated for
each class (except for some tree-based methods). See
[`caret::varImp`](https://rdrr.io/pkg/caret/man/varImp.html) for
details.

## See also

[`varImp`](https://rdrr.io/pkg/caret/man/varImp.html),
[`predict.train`](https://rdrr.io/pkg/caret/man/predict.train.html)

## Author

Zakaria Kehel, Bancy Ngatia, Khadija Aziz, Zainab Azough, Chafik Analy

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example for classification model
  data("septoriaDurumWC")
  knn.mod <- caret::train(ST_S ~ ., data = septoriaDurumWC, method = "knn")
  testdata <- septoriaDurumWC
  knn.varimp <- varimpPred(newdata = testdata, y = "ST_S", positive = "R", model = knn.mod)
  print(knn.varimp)

  # Example with SVM and ROC curve
  svm.mod <- caret::train(ST_S ~ ., data = septoriaDurumWC, method = "svmLinear2",
   metric = "Accuracy")
  testdata <- septoriaDurumWC
  svm.varimp <- varimpPred(newdata = testdata, y = "ST_S", positive = "R", model = svm.mod,
   auc = TRUE, predict = TRUE)
  print(svm.varimp)
} # }
```

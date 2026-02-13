# Automated Data Splitting, Tuning, and Model Training

A high-level wrapper for the `caret` framework that automates data
partitioning, two-stage hyperparameter tuning (coarse and refined), and
comprehensive model evaluation for both classification and regression.

## Usage

``` r
tuneTrain(
  data,
  y,
  p = 0.7,
  method = "knn",
  parallelComputing = FALSE,
  length = 10,
  control = "repeatedcv",
  number = 10,
  repeats = 10,
  process = c("center", "scale"),
  summary = multiClassSummary,
  positive = NULL,
  imbalanceMethod = NULL,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing the target variable and predictors.

- y:

  Character. The name of the target variable column in `data`.

- p:

  Numeric. Proportion of data used for training (0 \< p \< 1). Default:
  0.7.

- method:

  Character. The model type (e.g., "knn", "rf", "nnet"). Use
  `names(caret::getModelInfo())` for available options.

- parallelComputing:

  Logical. If `TRUE`, uses `doParallel` for multi-core processing.
  Default: `FALSE`.

- length:

  Integer. Initial number of tuning parameter combinations. Default: 10.

- control:

  Character. Resampling method (e.g., "cv", "repeatedcv"). Default:
  "repeatedcv".

- number:

  Integer. Number of folds or resampling iterations. Default: 10.

- repeats:

  Integer. Number of repeats for k-fold cross-validation. Default: 10.

- process:

  Character vector. Pre-processing steps (e.g., "center", "scale").
  Default: `c("center", "scale")`.

- summary:

  Function. Performance metric computation. Default:
  `multiClassSummary`.

- positive:

  Character. The positive class for binary classification. Default:
  `NULL`.

- imbalanceMethod:

  **\[experimental\]** Character. Subsampling method ("up" or "down").
  Default: `NULL`.

- ...:

  Additional arguments passed to
  [`trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html) and
  [`train`](https://rdrr.io/pkg/caret/man/train.html).

## Value

\#' @return A `list` containing:

- `Tuning`: Initial model tuning results.

- `Training`: Final model trained on optimized parameters.

- `ModelQuality`: Confusion Matrix (Classification) or Residual Metrics
  (Regression).

- `VariableImportance`: A plot of relative feature importance.

- `Predictions`: Predicted values for the test set.

- `ROC_Plot`: ROC plots (Classification).

- `Residuals vs. Predicted Plot`: Residual analysis (Regression).

- `Predicted vs. Actual Plot`: Actual vs Predicted values (Regression).

- `Training Data` / `Test Data`: The partitioned datasets.

## Details

The function automatically detects the task type:

- If `y` is a **factor**, classification is performed with class
  probability estimation.

- If `y` is **numeric**, regression is performed.

A two-stage tuning process is used for specific methods (KNN, RF, SVM,
NNET) where an initial coarse search is followed by a refined grid
search centered on the best initial parameters. '

## See also

[`trainControl`](https://rdrr.io/pkg/caret/man/trainControl.html),
[`train`](https://rdrr.io/pkg/caret/man/train.html),
[`predict.train`](https://rdrr.io/pkg/caret/man/predict.train.html)

## Author

Chafik Analy, Khadija Aziz, Zakaria Kehel, Bancy Ngatia, Khadija Aouzal

## Examples

``` r
if (FALSE) { # \dontrun{
# Loading test datasets
data(DurumWheatDHEWC)
data(BarleyRNOWC)
data(septoriaDurumWC)

## Binary classification of ST_S with balanced data
tree.ST_S <- tuneTrain(data = as.data.frame(septoriaDurumWC),
                     y =  'ST_S',
                     method = 'treebag',
                     summary = multiClassSummary,
                     repeats = 3, positive = 'R')

## Binary classification of RNO with imbalanced data
knn.RNO <- tuneTrain(data = BarleyRNOWC,
                     y =  'RNO',
                     method = 'knn',
                     summary = multiClassSummary,
                     process = c("scale","center"),
                     imbalanceMethod ="up",
                     repeats = 3)
                      
## Regression of DHE
svm.DHE <- tuneTrain(data = DurumWheatDHEWC,
                     y =  'DHE',
                     method = 'svmLinear2',
                     summary = defaultSummary,
                     repeats = 3)
 } # }
```

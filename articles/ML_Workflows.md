# Predictive Modeling using tuneTrain()

### Binary classification: balanced data

`septoriaDurumWC` dataset is used to illustrate the binary
classification of balanced data. This dataset includes monthly data for
3 climatic variables (tmin, tmax and precipitation), 19 bioclimatic
variables and evaluation of durum wheat accessions for Septoria Tritici.

The response to Septoria Tritici is labeled as `R` for `Resistant` and
`S` for `Susceptible`.

- **R (Resistant):** represents samples/observations that are resistant
  to Septoria. This level indicates the wheat genotypes or environmental
  conditions where Septoria’s impact is minimal or absent.
- **S (Susceptible):** represents samples/observations that are
  susceptible to Septoria. This level indicates the wheat genotypes or
  environmental conditions where Septoria’s impact is significant.

``` r
library(icardaFIGSr)
library(dplyr)
library(pROC)
library(caret)

# Load data sample
data("septoriaDurumWC")

# Check data is balanced
septoriaDurumWC |>
  count(ST_S)
#> # A tibble: 2 × 2
#>   ST_S      n
#>   <fct> <int>
#> 1 R       106
#> 2 S        94

## Run binary classification of ST_S with balanced data
knn.ST_S <- tuneTrain(data = as.data.frame(septoriaDurumWC),
                      y =  'ST_S',
                      method = 'knn', # using knn algorithm
                      summary = multiClassSummary, # Important for classification tasks
                      repeats = 3)
```

Let’s explore the results..

``` r
# Plot class probabilities
knn.ST_S$ProbabilitiesPlot
```

![Class Probabilities for ST_S using
KNN](ML_Workflows_files/figure-html/unnamed-chunk-3-1.png)

Class Probabilities for ST_S using KNN

``` r
# ROC plot
knn.ST_S$ROC_Plot
```

![Roc Plot for ST_S using
KNN](ML_Workflows_files/figure-html/unnamed-chunk-4-1.png)

Roc Plot for ST_S using KNN

``` r
# Variable importance
knn.ST_S$VariableImportance
```

![Variable Importance for ST_S using
KNN](ML_Workflows_files/figure-html/unnamed-chunk-5-1.png)

Variable Importance for ST_S using KNN

``` r
# Model quality
knn.ST_S$ModelQuality
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  R  S
#>          R 20  8
#>          S 11 20
#>                                           
#>                Accuracy : 0.678           
#>                  95% CI : (0.5436, 0.7938)
#>     No Information Rate : 0.5254          
#>     P-Value [Acc > NIR] : 0.01262         
#>                                           
#>                   Kappa : 0.3576          
#>                                           
#>  Mcnemar's Test P-Value : 0.64636         
#>                                           
#>             Sensitivity : 0.6452          
#>             Specificity : 0.7143          
#>          Pos Pred Value : 0.7143          
#>          Neg Pred Value : 0.6452          
#>              Prevalence : 0.5254          
#>          Detection Rate : 0.3390          
#>    Detection Prevalence : 0.4746          
#>       Balanced Accuracy : 0.6797          
#>                                           
#>        'Positive' Class : R               
#> 

# Training object
knn.ST_S$Training
#> k-Nearest Neighbors 
#> 
#> 141 samples
#>  55 predictor
#>   2 classes: 'R', 'S' 
#> 
#> Pre-processing: centered (55), scaled (55) 
#> Resampling: Cross-Validated (10 fold, repeated 3 times) 
#> Summary of sample sizes: 127, 127, 127, 128, 127, 126, ... 
#> Resampling results across tuning parameters:
#> 
#>   k  logLoss   AUC        prAUC      Accuracy   Kappa      F1       
#>   5  2.117061  0.6671840  0.4985782  0.6319414  0.2684773  0.6033431
#>   6  1.214363  0.6505385  0.4905817  0.6425031  0.2893426  0.6199563
#>   7  1.146843  0.6472222  0.4918258  0.6181319  0.2426935  0.5871552
#>   8  1.160049  0.6324192  0.4919960  0.6131868  0.2341963  0.5837480
#>   9  1.159630  0.6238308  0.4998371  0.6082418  0.2230264  0.5791040
#>   Sensitivity  Specificity  Pos_Pred_Value  Neg_Pred_Value  Precision
#>   0.5517857    0.7214286    0.7167328       0.5934367       0.7167328
#>   0.5690476    0.7253968    0.7253836       0.5990717       0.7253836
#>   0.5297619    0.7166667    0.7049206       0.5769481       0.7049206
#>   0.5351190    0.7031746    0.6975998       0.5739165       0.6975998
#>   0.5291667    0.6984127    0.6915873       0.5684466       0.6915873
#>   Recall     Detection_Rate  Balanced_Accuracy
#>   0.5517857  0.2940659       0.6366071        
#>   0.5690476  0.3030891       0.6472222        
#>   0.5297619  0.2820024       0.6232143        
#>   0.5351190  0.2843834       0.6191468        
#>   0.5291667  0.2819780       0.6137897        
#> 
#> Accuracy was used to select the optimal model using the largest value.
#> The final value used for the model was k = 6.

# Tuning object
knn.ST_S$Tuning
#> k-Nearest Neighbors 
#> 
#> 141 samples
#>  55 predictor
#>   2 classes: 'R', 'S' 
#> 
#> Pre-processing: centered (55), scaled (55) 
#> Resampling: Cross-Validated (10 fold, repeated 3 times) 
#> Summary of sample sizes: 127, 128, 128, 127, 126, 126, ... 
#> Resampling results across tuning parameters:
#> 
#>   k   logLoss    AUC        prAUC      Accuracy   Kappa      F1       
#>    5  2.2616126  0.6500709  0.4828789  0.6260562  0.2572795  0.6009026
#>    7  1.4423420  0.6460530  0.5016384  0.6289377  0.2653482  0.5986522
#>    9  1.2328555  0.6360261  0.5097475  0.6247009  0.2577766  0.5951461
#>   11  1.0782278  0.6340632  0.5203342  0.6095726  0.2262241  0.5750704
#>   13  0.6905751  0.6438209  0.5202541  0.6143590  0.2375240  0.5794117
#>   15  0.6884231  0.6386621  0.5269275  0.6119536  0.2314113  0.5749646
#>   17  0.6793789  0.6402778  0.5203508  0.6068254  0.2211182  0.5725103
#>   19  0.6735896  0.6417304  0.5173074  0.6117460  0.2347800  0.5645023
#>   21  0.6639009  0.6502480  0.5136864  0.6093407  0.2294590  0.5623978
#>   23  0.6628867  0.6429422  0.5271042  0.6024908  0.2168394  0.5522164
#>   Sensitivity  Specificity  Pos_Pred_Value  Neg_Pred_Value  Precision
#>   0.5541667    0.7047619    0.6948545       0.5858345       0.6948545
#>   0.5398810    0.7277778    0.7046693       0.5854978       0.7046693
#>   0.5315476    0.7293651    0.6987698       0.5804389       0.6987698
#>   0.5172619    0.7119048    0.6892593       0.5665308       0.6892593
#>   0.5172619    0.7238095    0.6871825       0.5720010       0.6871825
#>   0.5119048    0.7214286    0.6844048       0.5691414       0.6844048
#>   0.5166667    0.7055556    0.6712302       0.5682552       0.6712302
#>   0.4898810    0.7476190    0.6959524       0.5685281       0.6959524
#>   0.4851190    0.7468254    0.7012698       0.5632900       0.7012698
#>   0.4720238    0.7476190    0.7046429       0.5555568       0.7046429
#>   Recall     Detection_Rate  Balanced_Accuracy
#>   0.5541667  0.2938706       0.6294643        
#>   0.5398810  0.2867033       0.6338294        
#>   0.5315476  0.2819414       0.6304563        
#>   0.5172619  0.2748230       0.6145833        
#>   0.5172619  0.2746398       0.6205357        
#>   0.5119048  0.2722589       0.6166667        
#>   0.5166667  0.2744811       0.6111111        
#>   0.4898810  0.2603053       0.6187500        
#>   0.4851190  0.2577411       0.6159722        
#>   0.4720238  0.2507326       0.6098214        
#> 
#> Accuracy was used to select the optimal model using the largest value.
#> The final value used for the model was k = 7.
```

> *Note the difference between Model Training and Tuning objects at the
> end of the output. The final hyperparameter value is different (6 vs
> 7).*

``` r
# You can also access the training and testing datasets from the resulted list
knn.ST_S$`Training Data`
knn.ST_S$`Test Data` 
```

### Binary classification: imbalanced data

`BarleyRNOWC` dataset is used to illustrate the binary classification of
imbalanced data, with the purpose of modeling the response
classification of barley Kernel row number type to climate variables.

The response variable RNO categorizes barley into the following levels:

- **1 (Six-rowed):** represents barley genotypes with six distinct rows
  of kernels on the spike.
- **2 (Two-rowed):** represents barley genotypes with two distinct rows
  of kernels on the spike.
- **3 (Two-rowed - rudimentary florets):** represents two-rowed barley
  with underdeveloped or rudimentary florets.
- **4 (Irregular lateral florets):** represents barley genotypes with
  irregularly developed lateral florets on the spike.
- **5 (Irregular: 2 and 6 rows):** represents heterogeneous genotypes
  showing spikes with both two-rowed and six-rowed characteristics.
- **10 (Heterogeneous):** represents a genetically diverse group with
  mixed spike structures.

For this example, we use only the first 2 categories : `six-rowed`
(cl_1) and `two-rowed` (cl_2).

``` r
# Load sample data
data(BarleyRNOWC)

# Count classes for data imbalance check
BarleyRNOWC %>%
  count(RNO)
#>   RNO   n
#> 1   1 170
#> 2   2  30

## Binary classification of RNO
rf.RNO <- tuneTrain(data = BarleyRNOWC,
                      y = 'RNO',
                      method = 'rf',
                      summary = multiClassSummary,
                      imbalanceMethod = "up",
                      process = c("scale", "center"),
                      repeats = 3)

# same outputs of binary classification task
names(rf.RNO)
#> [1] "Tuning"             "Training"           "Training Data"     
#> [4] "Test Data"          "Predictions"        "VariableImportance"
#> [7] "ModelQuality"       "ProbabilitiesPlot"  "ROC_Plot"
```

``` r
## Plot class probabilities
rf.RNO$ProbabilitiesPlot
```

![Class Probabilities for RNO using Random
Forest](ML_Workflows_files/figure-html/unnamed-chunk-8-1.png)

Class Probabilities for RNO using Random Forest

``` r
## ROC plot
rf.RNO$ROC_Plot
```

![Roc Plot for RNO using Random
Forest](ML_Workflows_files/figure-html/unnamed-chunk-9-1.png)

Roc Plot for RNO using Random Forest

``` r
## Variable importance
rf.RNO$VariableImportance
```

![Variable Importance for RNO using Random
Forest](ML_Workflows_files/figure-html/unnamed-chunk-10-1.png)

Variable Importance for RNO using Random Forest

### Regression

`DurumWheatDHEWC` dataset is designed for modeling climate impacts on
the days to heading (DHE) of durum wheat. It contains multiple columns
representing climate variables, which are used as predictors, and a
numeric response variable, DHE, which indicates the number of days
required for durum wheat to reach the heading stage.

``` r
# Load sample data for regression task
data("DurumWheatDHEWC")

## Regression of DHE (days to heading)
svm.DHE <- tuneTrain(data = DurumWheatDHEWC,
                      y =  'DHE',
                      method = 'svmLinear2',
                      summary = defaultSummary,
                      repeats = 3)

# Regression outputs
names(svm.DHE)
#> [1] "Tuning"                       "Training"                    
#> [3] "Training Data"                "Test Data"                   
#> [5] "Predictions"                  "VariableImportance"          
#> [7] "QualityMetrics"               "Residuals vs. Predicted Plot"
#> [9] "Predicted vs. Actual Plot"
```

``` r
svm.DHE$VariableImportance
```

![Variable Importance for
DHE](ML_Workflows_files/figure-html/unnamed-chunk-12-1.png)

Variable Importance for DHE

``` r
svm.DHE$QualityMetrics
#>      RMSE  Rsquared       MAE 
#> 4.3359347 0.3971378 3.4593691
svm.DHE$`Predicted vs. Actual Plot`
```

![Predicted vs Actual
DHE](ML_Workflows_files/figure-html/unnamed-chunk-13-1.png)

Predicted vs Actual DHE

``` r
svm.DHE$`Residuals vs. Predicted Plot`
```

![Residuals vs predicted
DHE](ML_Workflows_files/figure-html/unnamed-chunk-14-1.png)

Residuals vs predicted DHE

### Multiclass classification

In this case, we use the same `DurumWheatDHEWC` dataset to create days
to heading classes variable (DHE_Class) to fit a multiclass model.
DHE_classes are descibed as follows:

- **1 (Early):** Represents samples/observations with early days to
  heading, indicating adaptability to shorter growing seasons or
  favorable early-season conditions.
- **2 (Intermediate):** Represents samples/observations with moderate
  days to heading, indicating typical or average responses under given
  environmental conditions.
- **3 (Late):** Represents samples/observations with late days to
  heading, suggesting adaptability to longer growing seasons or
  late-season conditions.

``` r
# Create DHE Classes from DurumWheatDHEWC dataset
DurumWheatDHEWC$DHE_class <- ifelse(
  DurumWheatDHEWC$DHE <= 172, "1",
  ifelse(DurumWheatDHEWC$DHE <= 180, "2", "3")
 )
 
# Convert to factor 
DurumWheatDHEWC$DHE_class <- factor(DurumWheatDHEWC$DHE_class)
   
# Count classes for data imbalance check
DurumWheatDHEWC %>%
   count(DHE_class)
#>   DHE_class  n
#> 1         1 96
#> 2         2 87
#> 3         3 10
```

``` r
## Run Multiclass Classification
rf.DHE_class <- tuneTrain(data = DurumWheatDHEWC,
                              y =  'DHE_class',
                              method = 'rf',
                              parallelComputing = FALSE,
                              summary = multiClassSummary,
                              imbalanceMethod ="up", # Here we upsample the less represented class (3)
                              repeats = 3)
# List returned objects
names(rf.DHE_class)
#>  [1] "Tuning"             "Training"           "Training Data"     
#>  [4] "Test Data"          "Predictions"        "VariableImportance"
#>  [7] "ModelQuality"       "ProbabilitiesPlot"  "AUC_Values"        
#> [10] "ROC_Plot"
rf.DHE_class$ModelQuality
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction X1 X2 X3
#>         X1 28  0  0
#>         X2  0 26  1
#>         X3  0  0  2
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.9825          
#>                  95% CI : (0.9061, 0.9996)
#>     No Information Rate : 0.4912          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.9676          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: X1 Class: X2 Class: X3
#> Sensitivity             1.0000    1.0000   0.66667
#> Specificity             1.0000    0.9677   1.00000
#> Pos Pred Value          1.0000    0.9630   1.00000
#> Neg Pred Value          1.0000    1.0000   0.98182
#> Prevalence              0.4912    0.4561   0.05263
#> Detection Rate          0.4912    0.4561   0.03509
#> Detection Prevalence    0.4912    0.4737   0.03509
#> Balanced Accuracy       1.0000    0.9839   0.83333
rf.DHE_class$ProbabilitiesPlot
```

![](ML_Workflows_files/figure-html/unnamed-chunk-16-1.png)

``` r
rf.DHE_class$ROC_Plot
```

![](ML_Workflows_files/figure-html/unnamed-chunk-16-2.png)

## splitData()

``` r

# Subset septoriaDurumWC where column names having 3, ex tmin3, prec13
septoriaDurumWC_subset <- icardaFIGSr::septoriaDurumWC|>
  dplyr::select(ST_S,contains("3"))

# split data
septoriaDurumWC_subset_split <- icardaFIGSr::splitData(septoriaDurumWC,
                        seed = 123, y="ST_S", p=0.7)

# Check results
names(septoriaDurumWC_subset_split)
#> [1] "trainset" "testset"
```

## getMetrics()

``` r

# Call the ST_S knn model fitted in tunTrain function section

data.test <- knn.ST_S$`Test Data`

pred.ST_S <- predict(knn.ST_S$Tuning, newdata = data.test[ , -1])

metrics.knn.ST_S <- getMetrics(y = data.test$ST_S,
                                       yhat = pred.ST_S, classtype = 2)

metrics.knn.ST_S
#> $Metrics
#>                            Metrics
#> Accuracy                     0.695
#> 95% CI              (0.561, 0.808)
#> No Information Rate          0.525
#> P-Value [Acc > NIR]    0.006076751
#> Kappa                         0.39
#> Sensitivity                  0.677
#> Specificity                  0.714
#> 
#> $CM
#>    R  S
#> R 21  8
#> S 10 20
```

## getMetricsPCA()

Please run below code chunk to test
[`getMetricsPCA()`](../reference/getMetricsPCA.md) on any model rutrned
by [`tuneTrain()`](../reference/tuneTrain.md).

``` r

## Run Binary classification of ST_S with balanced data using random forest
rf.ST_S <- tuneTrain(
                      data = as.data.frame(septoriaDurumWC),
                      y =  'ST_S',
                      method = 'rf', # using rf algorithm
                      summary = multiClassSummary, # Important for classification tasks  
                      parallelComputing = FALSE,
                      repeats = 3,
                      process = c("center","scale"))

# get test data from one of the model to be used for prediction
data.test <- rf.ST_S$`Test Data`

# Obtain predictions from previously run models : knn.ST_S and rf.ST_S
pred.knn.ST_S <- predict(knn.ST_S$Tuning, newdata = data.test[ , -1])
pred.rf.ST_S <- predict(rf.ST_S$Tuning, newdata = data.test[ , -1])

# Get metrics for your model using computed metrics.knn.ST_S
metrics.knn.ST_S <- metrics.knn.ST_S
metrics.rf.ST_S <- icardaFIGSr::getMetrics(y = data.test$ST_S,
                         yhat = pred.rf.ST_S, classtype = 2)

# Indexing for 2-class models to remove extra column with
# names of performance measures
metrics.all <- cbind(metrics.knn.ST_S, metrics.rf.ST_S)
  
## check data structure
metrics.all
#>         metrics.knn.ST_S metrics.rf.ST_S
#> Metrics data.frame,1     data.frame,1   
#> CM      data.frame,2     data.frame,2
```

``` r
metrics.all[1,1]
#> [[1]]
#>                            Metrics
#> Accuracy                     0.695
#> 95% CI              (0.561, 0.808)
#> No Information Rate          0.525
#> P-Value [Acc > NIR]    0.006076751
#> Kappa                         0.39
#> Sensitivity                  0.677
#> Specificity                  0.714
```

``` r
metrics.all[2,1]
#> [[1]]
#>    R  S
#> R 21  8
#> S 10 20
```

## make_prediction()

[`make_prediction()`](../reference/make_prediction.md) returns
predictions or classes probabilities and corresponding plots.

``` r
# Make prediction for septoriaDurumWC using the previous tuning model knn.ST_S$Tuning

data("septoriaDurumWC")

septoriaDurumWC <- as.data.frame(septoriaDurumWC)

knn.pred <- make_prediction(newdata = septoriaDurumWC,
                                      y='ST_S',
                                      model = knn.ST_S$Tuning,
                                      positive = "R",
                                      auc = TRUE)

names(knn.pred)
#> [1] "ClassProbabilities"     "ClassProbabilitiesPlot" "ROC_Curves"            
#> [4] "AUC"
```

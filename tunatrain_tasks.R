library(caret)
library(e1071)       
library(randomForest)
library(nnet)         
library(ggplot2)    

# Binary classification
tuneTrainBinaryClassification <- function(data, target, method = "knn",
                                          preProcessOpts = c("center", "scale"),
                                          metricToUse = "Accuracy" ,...) {
  
  
  
  # Ensure target is a factor for classification
  data[[target]] <- as.factor(data[[target]])
  
  # Split data into training and testing sets
  set.seed(123) # For reproducibility
  trainIndex <- createDataPartition(data[[target]], p = 0.7, list = FALSE)
  trainSet <- data[trainIndex, ]
  testSet <- data[-trainIndex, ]
  
  # Preprocess data
  preProcValues <- preProcess(trainSet[, -which(names(trainSet) == target)], method = preProcessOpts)
  trainSet <- predict(preProcValues, trainSet)
  testSet <- predict(preProcValues, testSet)
  
  # Define train control
  trainControlObj <- trainControl(method = "repeatedcv", number = 10, repeats = 5,
                                  sampling = "up",
                                  classProbs = TRUE, summaryFunction = defaultSummary)
  
  # Dynamic hyperparameter tuning grid setup
  tuneGrid <- NULL
  nFeatures <- ncol(trainSet) - 1
  if (method == "knn") {
    tuneGrid <- expand.grid(k = seq(1, min(21, nFeatures), by = 1))
  } else if (method == "svmLinear2") {
    tuneGrid <- expand.grid(C = 2^seq(-5, 2, length.out = 5))
  } else if (method == "rf") {
    tuneGrid <- expand.grid(.mtry = seq(2, max(2, floor(sqrt(nFeatures))), by = 2))
  } else if (method == "nnet") {
    maxNodes <- min(15, floor(sqrt(nrow(trainSet))))
    tuneGrid <- expand.grid(size = seq(1, maxNodes, length.out = 5), decay = c(0, 0.001, 0.01, 0.1))
  }
  
  # Model training with specified method and hyperparameters
  model <- train(reformulate(".", target), data = trainSet, method = method,
                 tuneGrid = tuneGrid, trControl = trainControlObj,
                 metric = metricToUse)
  
  # Prediction and evaluation
  predictions <- predict(model, newdata = testSet)
  prob <- as.data.frame(caret::predict.train(model, newdata = testSet, type = "prob"))
  confusionMat <- confusionMatrix(predictions, testSet[[target]], mode="everything")
  
  # Generate a long format dataframe suitable for plotting
  prob <- utils::stack(prob)
  colnames(prob) <- c("Probability", "Class")
  
  # Generate histogram for class probabilities
  Probability_plot <- ggplot(prob, aes(x = Probability, group = Class, colour = Class, fill = Class)) +
    geom_histogram(alpha = 0.4, position = "identity", binwidth = 0.1) +
    theme_bw() +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    facet_wrap(~Class, scales = "free") +
    labs(x = "Predicted Probability", y = "Count", title = "Histogram of Predicted Class Probabilities")

  # Return results
  return(list(Model = model, Predictions = predictions,
              Probability_plot = Probability_plot,
              Probabilities = prob, ConfusionMatrix = confusionMat))
}

## Regression
tuneTrainRegression <- function(data, target, method = "rf",
                                preProcessOpts = c("center", "scale"),
                                ...) {
  # Ensure target is numeric for regression
  if (!is.numeric(data[[target]])) {
    stop("Target variable must be numeric for regression.")
  }
  
  # Split data into training and testing sets
  set.seed(123) # For reproducibility
  trainIndex <- createDataPartition(data[[target]], p = 0.7, list = FALSE)
  trainSet <- data[trainIndex, ]
  testSet <- data[-trainIndex, ]
  
  # Preprocess data
  preProcValues <- preProcess(trainSet[, -which(names(trainSet) == target)], method = preProcessOpts)
  trainSet <- predict(preProcValues, trainSet)
  testSet <- predict(preProcValues, testSet)
  
  # Define train control for regression
  trainControlObj <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                                  summaryFunction = defaultSummary)
  
  # Dynamic hyperparameter tuning grid setup based on method
  tuneGrid <- NULL
  nFeatures <- ncol(trainSet) - 1
  if (method == "rf") {
    tuneGrid <- expand.grid(.mtry = seq(1, max(1, floor(sqrt(nFeatures))), by = 1))
  } else if (method == "nnet") {
    maxNodes <- min(15, floor(sqrt(nrow(trainSet)/2)))
    tuneGrid <- expand.grid(size = seq(1, maxNodes, length.out = 5), decay = c(0, 0.001, 0.01, 0.1))
  } else if(method == "knn") {
    tuneGrid <- expand.grid(k = seq(1, min(21, nFeatures), by = 1))
  } else if (method == "svmLinear2") {
    tuneGrid <- expand.grid(C = 2^seq(-5, 2, length.out = 5))
  }
  
  # Model training
  model <- train(reformulate(".", target), data = trainSet, method = method,
                 tuneGrid = tuneGrid, trControl = trainControlObj)
  
  # Predictions for evaluation
  predictions <- predict(model, testSet)
  
  # Evaluate model performance with RMSE, Rsquared, and MAE
  actual <- testSet[[target]]
  rmse <- sqrt(mean((predictions - actual)^2))
  rsquared <- cor(predictions, actual)^2
  mae <- mean(abs(predictions - actual))
  
  Quality_metrics <- postResample(pred = predictions,
                                  obs = actual)
  
  # Return model, predictions, and evaluation metrics
  return(list(Model = model, Predictions = predictions,
              Quality_metrics = Quality_metrics,
              RMSE = rmse, Rsquared = rsquared, MAE = mae))
}

## Testing examples

# Regression Example
DHE.knn.S <- tuneTrainRegression(data = DurumWheatDHEWC,
                                 target = "DHE",
                                 method = "knn")

# Classification Example with imbalance data
RNO.knn.S <- tuneTrainBinaryClassification(data = BarleyRNOWC,
                                           target = "RNO",
                                           method = "knn")


## Visualize durum metadata

library(leaflet)
library(htmltools)
library(dplyr)

data(durum)

leaflet(durum %>%
          filter(!is.na(Altitude))) %>%
  addTiles() %>%
  addMarkers(
    lng = ~Longitude, 
    lat = ~Latitude, 
    popup = ~paste("Population type: ", htmlEscape(PopulationType), "<br>",
                   "Altitude: ", htmlEscape(Altitude), "<br>", 
                   "SiteCode: ", htmlEscape(SiteCode)),  
    clusterOptions = markerClusterOptions()
  )





## Multiclass classification
# tuneTrainMulticlassClassification <- function(data, target, method = "rf",
#                                               preProcessOpts = c("center", "scale"),
#                                               ...) {
#   # Ensure target is a factor for classification
#   data[[target]] <- as.factor(data[[target]])
# 
#   # Split data into training and testing sets
#   set.seed(123) # For reproducibility
#   trainIndex <- createDataPartition(data[[target]], p = 0.7, list = FALSE)
#   trainSet <- data[trainIndex, ]
#   testSet <- data[-trainIndex, ]
# 
#   # Preprocess data
#   preProcValues <- preProcess(trainSet[, -which(names(trainSet) == target)], method = preProcessOpts)
#   trainSet <- predict(preProcValues, trainSet)
#   testSet <- predict(preProcValues, testSet)
# 
#   # Define train control for multiclass with class probabilities
#   trainControlObj <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
#                                   classProbs = TRUE, summaryFunction = multiClassSummary)
# 
#   # Dynamic hyperparameter tuning grid setup based on method
#   tuneGrid <- NULL
#   nFeatures <- ncol(trainSet) - 1
#   if (method == "knn") {
#     tuneGrid <- expand.grid(k = seq(1, min(21, nFeatures), by = 2))
#   } else if (method == "svmLinear2") {
#     tuneGrid <- expand.grid(C = 2^seq(-5, 2, length.out = 5))
#   } else if (method == "rf") {
#     tuneGrid <- expand.grid(.mtry = seq(2, max(2, floor(sqrt(nFeatures))), by = 2))
#   } else if (method == "nnet") {
#     maxNodes <- min(15, floor(sqrt(nrow(trainSet))))
#     tuneGrid <- expand.grid(size = seq(1, maxNodes, length.out = 5), decay = c(0, 0.001, 0.01, 0.1))
#   }
# 
#   # Model training
#   model <- train(reformulate(".", target), data = trainSet, method = method,
#                  tuneGrid = tuneGrid, trControl = trainControlObj)
# 
#   # Predict probabilities for multiclass
#   prob.mod <- predict(model, testSet, type = "prob")
# 
#   # Convert probabilities to long format for ggplot
#   prob.newdf <- reshape2::melt(as.data.frame(prob.mod))
#   names(prob.newdf) <- c("Class", "Probability")
# 
#   # Plotting probabilities
#   prob.hist <- ggplot(prob.newdf, aes(x = Probability, fill = Class)) +
#     geom_histogram(alpha = 0.4, position = "identity", binwidth = 0.05) +
#     facet_wrap(~Class, scales = "free") +
#     theme_bw() +
#     labs(title = "Predicted Class Probabilities", x = "Probability", y = "Count")
# 
#   # Display the plot
#   print(prob.hist)
# 
#   # Return model and its evaluation
#   return(list(Model = model, ProbabilitiesPlot = prob.hist))
# }

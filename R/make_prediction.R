#' @title Make Predictions
#' @description `make_prediction` makes predictions.
#'  It returns a list containing a data frame of predictions or class probabilities, and corresponding plots.
#' @param newdata A data frame. The test dataset.
#' @param y The name of the target variable.
#' @param positive The positive class for the target variable if `y` is a factor.
#'  Default: `NULL`.
#' @param model A model object. The trained model returned by `caret::train`.
#' @param scale A logical value. If `TRUE`, scales the variable importance values to between 0-100.
#'  Default: `FALSE`.
#' @param auc A logical value.
#'  If `TRUE`, calculates the area under the ROC curve (AUC) for classification models.
#'  Default: `FALSE`.
#' @return A list containing predictions, class probabilities, and corresponding plots.
#'
#' - For classification models, class probabilities and ROC curves are included.
#' - For regression models, predictions and residuals vs. predicted plots are included.
#'
#' @details This function uses `caret`, `ggplot2`, and `plotROC` for calculations and plotting.
#'
#' @author Zakaria Kehel, Bancy Ngatia, Khadija Aziz, Zainab Azough, Chafik Analy, Khadija Aouzal
#' @examples
#' \dontrun{
#'   # Example for classification model
#'   data("septoriaDurumWC")
#'   knn.mod <- caret::train(ST_S ~ ., data = septoriaDurumWC, method = "knn")
#'   testdata <- septoriaDurumWC
#'   knn.pred <- make_prediction(newdata = testdata, y = "ST_S", positive = "R", model = knn.mod)
#'   print(knn.pred)
#'
#'   # Example with SVM and ROC curve
#'   svm.mod <- caret::train(ST_S ~ ., data = septoriaDurumWC, method = "svmLinear2",
#'    metric = "Accuracy")
#'   testdata <- septoriaDurumWC
#'   svm.pred <- make_prediction(newdata = testdata, y = "ST_S", positive = "R", model = svm.mod,
#'    auc = TRUE)
#'   print(svm.pred)
#' }
#' @seealso \code{\link[caret]{predict.train}}
#' @name make_prediction
#' @importFrom caret predict.train
#' @importFrom utils stack
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes geom_histogram theme_bw scale_colour_brewer scale_fill_brewer labs coord_equal annotate geom_point
#' @importFrom plotROC geom_roc style_roc calc_auc
#' @export

make_prediction <- function(newdata, y, positive = NULL, model, scale = FALSE, auc = FALSE) {
  # Validate inputs
  if (missing(newdata) || !is.data.frame(newdata)) {
    stop("'newdata' must be a data frame.")
  }
  if (missing(y) || !is.character(y)) {
    stop("'y' must be a character string specifying the target variable.")
  }
  if (missing(model)) {
    stop("Please provide a trained model in 'model'.")
  }
  
  yvec <- newdata[[y]]
  
  # Classification case
  if (is.factor(yvec)) {
      prob.mod <- as.data.frame(caret::predict.train(model, newdata, type = "prob"))
      roc_curves <- list()
      auc_values <- list()
      
      if (auc) {
        for (class_name in colnames(prob.mod)) {
          roc_obj <- pROC::roc(
            yvec,
            prob.mod[[class_name]],
            levels = rev(levels(yvec)),
            direction = "<"
          )
          auc_values[[class_name]] <- round(pROC::auc(roc_obj), 4)
          roc_curves[[class_name]] <- pROC::ggroc(roc_obj) +
            ggplot2::labs(title = paste("ROC Curve for", class_name, "AUC:", auc_values[[class_name]])) +
            ggplot2::theme_minimal()
        }
      }
    
    prob.newdf <- utils::stack(prob.mod)
    colnames(prob.newdf) <- c("Probability", "Class")
    prob.plot <- ggplot2::ggplot(prob.newdf, ggplot2::aes(x = Probability, fill = Class, colour = Class)) +
      ggplot2::geom_histogram(alpha = 0.4, size = 1, position = "identity", binwidth = 0.05) +
      ggplot2::theme_bw() +
      ggplot2::scale_colour_brewer(palette = "Dark2") +
      ggplot2::scale_fill_brewer(palette = "Dark2") +
      ggplot2::labs(y = "Count")
    
    return(list(
      ClassProbabilities = prob.mod,
      ClassProbabilitiesPlot = prob.plot,
      ROC_Curves = if (auc) roc_curves else NULL,
      AUC = if (auc) auc_values else NULL
    ))
  }
  
  # Regression case
  else if (is.numeric(yvec)) {
    
      pred.mod <- caret::predict.train(model, newdata, type = "raw")
      resids <- newdata[[y]] - pred.mod
      respred.df <- data.frame(Residuals = resids, Predicted = pred.mod)
      respred.plot <- ggplot2::ggplot(respred.df, ggplot2::aes(x = Predicted, y = Residuals)) +
        ggplot2::geom_point(color = "blue") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Residuals vs Predicted", x = "Predicted", y = "Residuals")
      return(list(
        Predictions = pred.mod,
        ResidualsVsPredictedPlot = respred.plot
      ))
  }
  
  else {
    stop("Unsupported response variable type. 'y' must be numeric or a factor.")
  }
}


#' @title Variable Importance and Predictions
#' @description `varimpPred` calculates variable importance and makes predictions. It returns a list containing a data frame of variable importance scores, predictions or class probabilities, and corresponding plots.
#' @param newdata A data frame. The test dataset.
#' @param y A character string. The name of the target variable.
#' @param positive A character string. The positive class for the target variable if `y` is a factor. Default: `NULL`.
#' @param model A model object. The trained model returned by `caret::train`.
#' @param scale A logical value. If `TRUE`, scales the variable importance values to between 0-100. Default: `FALSE`.
#' @param auc A logical value. If `TRUE`, calculates the area under the ROC curve (AUC) for classification models. Default: `FALSE`.
#' @param predict A logical value. If `TRUE`, calculates predictions or class probabilities. Default: `FALSE`.
#' @param type A character string. The type of prediction, e.g., `"prob"` or `"raw"`. Default: `"prob"`.
#' @param ... Additional arguments to be passed to `caret::varImp`.
#' @return A list containing variable importance scores, predictions, class probabilities, and corresponding plots.
#'
#' - For classification models, class probabilities and ROC curves are included if `predict = TRUE`.
#' - For regression models, predictions and residuals vs. predicted plots are included if `predict = TRUE`.
#'
#' @details This function uses `caret`, `ggplot2`, and `plotROC` for calculations and plotting. Variable importance is calculated based on the type of model.
#'
#' For example, for regression models, the absolute value of the t-statistic of each parameter is used in the importance calculation.
#' For classification models, a variable importance score is calculated for each class (except for some tree-based methods). 
#' See `caret::varImp` for details.
#' @author Zakaria Kehel, Bancy Ngatia, Khadija Aziz, Zainab Azough, Chafik Analy
#' @examples
#' \dontrun{
#'   # Example for classification model
#'   data("septoriaDurumWC")
#'   knn.mod <- caret::train(ST_S ~ ., data = septoriaDurumWC, method = "knn")
#'   testdata <- septoriaDurumWC
#'   knn.varimp <- varimpPred(newdata = testdata, y = "ST_S", positive = "R", model = knn.mod)
#'   print(knn.varimp)
#'
#'   # Example with SVM and ROC curve
#'   svm.mod <- caret::train(ST_S ~ ., data = septoriaDurumWC, method = "svmLinear2",
#'    metric = "Accuracy")
#'   testdata <- septoriaDurumWC
#'   svm.varimp <- varimpPred(newdata = testdata, y = "ST_S", positive = "R", model = svm.mod,
#'    auc = TRUE, predict = TRUE)
#'   print(svm.varimp)
#' }
#' @seealso \code{\link[caret]{varImp}}, \code{\link[caret]{predict.train}}
#' @name varimpPred
#' @importFrom caret varImp predict.train
#' @importFrom utils stack
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes geom_histogram theme_bw scale_colour_brewer scale_fill_brewer labs coord_equal annotate geom_point
#' @importFrom plotROC geom_roc style_roc calc_auc
#' @export

varimpPred <- function(newdata, y, positive = NULL, model, scale = FALSE, auc = FALSE, predict = FALSE, type = "prob", ...) {
  # Deprecation message using lifecycle::deprecate_warn()
  lifecycle::deprecate_warn("2.0.0", "varimpPred()", "make_prediction()",
                            details = paste(
                              "The new function `make_prediction()` now returns only predictions. Variable importance scores can be extracted from `tuneTrain()` result.",
                              "To avoid breaking code, `varimpPred()` will still work in this version (2.0.0).",
                              "However, it is recommended to update your code to use `make_prediction()` to make predictions and `tuneTrain()` to extract variable importance scores."
                            ))
  # Validate inputs
  if (missing(newdata) || !is.data.frame(newdata)) {
    stop("'newdata' must be a data frame.")
  }
  if (missing(y) || !is.character(y)) {
    stop("'y' must be a character string specifying the target variable.")
  }
  if (missing(model)) {
    stop("Please provide a trained model in 'model'.")
  }
  
  yvec <- newdata[[y]]
  
  # Classification case
  if (is.factor(yvec)) {
    if (predict) {
      prob.mod <- as.data.frame(caret::predict.train(model, newdata, type = type))
      roc_curves <- list()
      auc_values <- list()
      
      if (auc) {
        for (class_name in colnames(prob.mod)) {
          roc_obj <- pROC::roc(
            yvec,
            prob.mod[[class_name]],
            levels = rev(levels(yvec)),
            direction = "<"
          )
          auc_values[[class_name]] <- round(pROC::auc(roc_obj), 4)
          roc_curves[[class_name]] <- pROC::ggroc(roc_obj) +
            ggplot2::labs(title = paste("ROC Curve for", class_name, "AUC:", auc_values[[class_name]])) +
            ggplot2::theme_minimal()
        }
      }
    }
    
    varimp <- caret::varImp(model, scale = scale, ...)
    varimp.mod <- data.frame(
      Variable = rownames(varimp$importance),
      Importance = varimp$importance[, positive]
    )
    plot.varimp <- ggplot2::ggplot(varimp.mod, ggplot2::aes(x = reorder(Variable, Importance), y = Importance)) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
      ggplot2::theme_minimal() +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Variable Importance", x = "Variable", y = "Importance")
    
    prob.newdf <- utils::stack(prob.mod)
    colnames(prob.newdf) <- c("Probability", "Class")
    prob.plot <- ggplot2::ggplot(prob.newdf, ggplot2::aes(x = Probability, fill = Class, colour = Class)) +
      ggplot2::geom_histogram(alpha = 0.4, size = 1, position = "identity", binwidth = 0.05) +
      ggplot2::theme_bw() +
      ggplot2::scale_colour_brewer(palette = "Dark2") +
      ggplot2::scale_fill_brewer(palette = "Dark2") +
      ggplot2::labs(y = "Count")
    
    return(list(
      VariableImportance = varimp.mod,
      VariableImportancePlot = plot.varimp,
      ClassProbabilities = prob.mod,
      ClassProbabilitiesPlot = prob.plot,
      ROC_Curves = if (auc) roc_curves else NULL,
      AUC = if (auc) auc_values else NULL
    ))
  }
  
  # Regression case
  else if (is.numeric(yvec)) {
    varimp <- caret::varImp(model, scale = scale, ...)
    varimp.mod <- data.frame(
      Variable = rownames(varimp$importance),
      Importance = varimp$importance[, 1]
    )
    
    plot.varimp <- ggplot2::ggplot(varimp.mod, ggplot2::aes(x = reorder(Variable, Importance), y = Importance)) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
      ggplot2::theme_minimal() +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Variable Importance", x = "Variable", y = "Importance")
    
    if (predict) {
      pred.mod <- caret::predict.train(model, newdata, type = "raw")
      resids <- newdata[[y]] - pred.mod
      respred.df <- data.frame(Residuals = resids, Predicted = pred.mod)
      respred.plot <- ggplot2::ggplot(respred.df, ggplot2::aes(x = Predicted, y = Residuals)) +
        ggplot2::geom_point(color = "blue") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Residuals vs Predicted", x = "Predicted", y = "Residuals")
      return(list(
        VariableImportance = varimp.mod,
        VariableImportancePlot = plot.varimp,
        Predictions = pred.mod,
        ResidualsVsPredictedPlot = respred.plot
      ))
    }
    
    return(list(
      VariableImportance = varimp.mod,
      VariableImportancePlot = plot.varimp
    ))
  }
  
  else {
    stop("Unsupported response variable type. 'y' must be numeric or a factor.")
  }
  
}

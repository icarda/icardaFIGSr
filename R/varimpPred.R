
#' @title Variable Importance and Predictions
#' @description varimpPred calculates Variable Importance and makes predictions, it returns a list containing a data frame of variable importance scores, predictions or class probabilities, and corresponding plots.
#' @param newdata object of class "data.frame" having test data.
#' @param y character. Target variable.
#' @param positive character. The positive class for the target variable if y is factor. Usually, it is the first level of the factor.
#' @param model expression. The model object returned after training a model on training data.
#' @param scale boolean. If \code{TRUE}, scales the variable importance values to between 0-100. Default: FALSE.
#' @param auc boolean. If \code{TRUE}, calculates the area under the ROC curve and returns the value. Default: FALSE.
#' @param predict boolean. If \code{TRUE}, calculates class probabilities and returns them as a data frame. Default: FALSE
#' @param ... additional arguments to be passed to \code{varImp} function in the package \code{caret}.
#' @return A list object with importance measures for variables in \code{newdata}, predictions for regression models, class probabilities for classification models, and corresponding plots.
#'
#' \code{newdata} should be either the test data that remains after splitting whole data into training and test sets, or a new data set different from the one used to train the model.
#'
#' If \code{y} is factor, class probabilities are calculated for each class. If \code{y} is numeric, predicted values are calculated.
#'
#' A ROC curve is created if \code{predict = TRUE} and \code{y} is factor. Otherwise, a plot of residuals versus predicted values is created if \code{y} is numeric.
#'
#' \code{varimpPred} relies on packages \code{caret}, \code{ggplot2} and \code{plotROC} to perform the calculations and plotting.
#' @details The importance measure for each variable is calculated based on the type of model.
#'
#' For example for linear models, the absolute value of the t-statistic of each parameter is used in the importance calculation.
#'
#' For classification models, with the exception of classification trees, bagged trees and boosted trees, a variable importance score is calculated for each class. See \code{\link[caret]{varImp}} for details on model-specific metrics.
#'
#' \code{varimpPred} can be used to obtain either variable importance metrics, predictions, class probabilities, or a combination of these.
#'
#' For classification models with \code{predict = TRUE}, class probabilities and ROC curve are given in the results.
#'
#' For regression models with \code{predict = TRUE}, predictions and residuals versus predicted plot are given.
#' @author Zakaria Kehel, Bancy Ngatia, Khadija Aziz, Zainab Azough
#' @examples
#' if(interactive()){
#'  # Calculate variable importance for classification model
#'  data("septoriaDurumWC")
#'  knn.mod <- tuneTrain(data = septoriaDurumWC,y = 'ST_S',method = 'knn')
#'  testdata <- knn.mod$`Test Data`
#'  knn.varimp<- varimpPred(newdata = testdata, y='ST_S', positive = 'R', model = knn.mod$Model)
#'  knn.varimp
#'  
#'  # Calculate variable importance and obtain class probabilities
#'  data("septoriaDurumWC")
#'  svm.mod <- tuneTrain(data = septoriaDurumWC, y = 'ST_S',method = 'svmLinear2',
#'                    predict = TRUE, positive = 'R',summary = twoClassSummary)
#'  testdata <- svm.mod$`Test Data`
#'  svm.varimp <- varimpPred(newdata = testdata, y = 'ST_S',
#'                           positive = 'R', model = svm.mod$Model,
#'                           ROC = TRUE, predict = TRUE)
#'  svm.varimp
#'  # Obtain variable importance plot for only first 20 variables
#'  # with highest measure
#'  svm.varimp <- varimpPred(newdata = testdata, y = 'ST_S',
#'                           positive = 'R', model = svm.mod$Model,
#'                           ROC = TRUE, predict = TRUE, top = 20)
#'  svm.varimp
#'  }
#' @seealso
#'  \code{\link[caret]{varImp}},
#'  \code{\link[caret]{predict.train}},
#'  \code{\link[ggplot2]{ggplot}},
#'  \code{\link[plotROC]{geom_roc}},
#'  \code{\link[plotROC]{calc_auc}}
#' @rdname varimpPred
#' @export
#' @importFrom caret varImp predict.train
#' @importFrom utils stack
#' @importFrom ggplot2 ggplot aes geom_histogram theme_bw scale_colour_brewer scale_fill_brewer labs coord_equal annotate geom_point
#' @importFrom plotROC geom_roc style_roc calc_auc

varimpPred <- function(newdata, y, positive, model, scale = FALSE, auc = FALSE, predict = FALSE, ...) {

  yvec = newdata[[y]]

  if(is.factor(yvec)) {
    if(predict & auc) {
      varimp = caret::varImp(model, scale = scale, ...)
      varimp.mod = data.frame(rownames(varimp$importance), matrix(unlist(varimp$importance), nrow = nrow(varimp$importance), byrow = TRUE))
      varimp.mod$VariableName = rownames(varimp.mod)
      colnames(varimp.mod) = c("Variable", names(varimp$importance)[1], names(varimp$importance)[2])
      varimp.mod = varimp.mod[ , -4]
      plot.varimp = plot(varimp, main = "Variable Importance", ...)
      prob.mod = as.data.frame(caret::predict.train(model, newdata, type = "prob"))
      prob.newdf = utils::stack(prob.mod)
      colnames(prob.newdf) = c("Probability", "Class")
      prob.hist = ggplot2::ggplot(prob.newdf, ggplot2::aes(x = Probability, colour = Class, fill = Class))
      prob.plot = prob.hist + ggplot2::geom_histogram(alpha = 0.4, size = 1, position = "identity") + ggplot2::theme_bw() + ggplot2::scale_colour_brewer(palette = "Dark2") + ggplot2::scale_fill_brewer(palette = "Dark2") + ggplot2::labs(y = "Count")
      negative = prob.mod[ , !names(prob.mod) %in% positive]
      g1 = ggplot2::ggplot(prob.mod, ggplot2::aes(m = negative, d = yvec)) + plotROC::geom_roc(n.cuts = 0) + ggplot2::coord_equal() + plotROC::style_roc()
      plot.roc = g1 + ggplot2::annotate("text", x = 0.75, y = 0.25, label = paste("AUC =", round(plotROC::calc_auc(g1)$AUC, 4)))
      auc = round(plotROC::calc_auc(g1)$AUC, 4)
      x = list("Variable Importance" = varimp.mod, "Variable Importance Plot" = plot.varimp, "Class Probabilities" = prob.mod, "Class Probabilities Plot" = prob.plot, "Area Under ROC Curve" = auc, "ROC Curve" = plot.roc)
      return(x)
    }

    else if(!predict & auc) {
      varimp = caret::varImp(model, scale = scale, ...)
      varimp.mod = data.frame(rownames(varimp$importance), matrix(unlist(varimp$importance), nrow = nrow(varimp$importance), byrow = TRUE))
    varimp.mod$VariableName = rownames(varimp.mod)
      colnames(varimp.mod) = c("Variable", names(varimp$importance)[1], names(varimp$importance)[2])
      varimp.mod = varimp.mod[ , -4]
      plot.varimp = plot(varimp, main = "Variable Importance", ...)
      prob.mod = as.data.frame(caret::predict.train(model, newdata, type = "prob"))
      negative = prob.mod[ , !names(prob.mod) %in% positive]
      g1 = ggplot2::ggplot(prob.mod, ggplot2::aes(m = negative, d = yvec)) + plotROC::geom_roc(n.cuts = 0) + ggplot2::coord_equal() + plotROC::style_roc()
      plot.roc = g1 + ggplot2::annotate("text", x = 0.75, y = 0.25, label = paste("AUC =", round(plotROC::calc_auc(g1)$AUC, 4)))
      auc = round(plotROC::calc_auc(g1)$AUC, 4)
      x = list("Variable Importance" = varimp.mod, "Variable Importance Plot" = plot.varimp, "Area Under ROC Curve" = auc, "ROC Curve" = plot.roc)
      return(x)
    }

    else if(predict & !auc) {
      varimp = caret::varImp(model, scale = scale, ...)
      varimp.mod = data.frame(rownames(varimp$importance), matrix(unlist(varimp$importance), nrow = nrow(varimp$importance), byrow = TRUE))
      varimp.mod$VariableName = rownames(varimp.mod)
      colnames(varimp.mod) = c("Variable", names(varimp$importance)[1], names(varimp$importance)[2])
      varimp.mod = varimp.mod[ , -4]
      plot.varimp = plot(varimp, main = "Variable Importance", ...)
      prob.mod = caret::predict.train(model, newdata, type = "prob")
      prob.newdf = utils::stack(prob.mod)
      colnames(prob.newdf) = c("Probability", "Class")
      prob.hist = ggplot2::ggplot(prob.newdf, ggplot2::aes(x = Probability, colour = Class, fill = Class))
      prob.plot = prob.hist + ggplot2::geom_histogram(alpha = 0.4, size = 1, position = "identity") + ggplot2::theme_bw() + ggplot2::scale_colour_brewer(palette = "Dark2") + ggplot2::scale_fill_brewer(palette = "Dark2") + ggplot2::labs(y = "Count")
      x = list("Variable Importance" = varimp.mod, "Variable Importance Plot" = plot.varimp, "Class Probabilities" = prob.mod, "Class Probabilities Plot" = prob.plot)
      return(x)
    }

    else if(!predict & !auc) {
      varimp = caret::varImp(model, scale = scale, ...)
      varimp.mod = data.frame(rownames(varimp$importance), matrix(unlist(varimp$importance), nrow = nrow(varimp$importance), byrow = TRUE))
      varimp.mod$VariableName = rownames(varimp.mod)
      colnames(varimp.mod) = c("Variable", names(varimp$importance)[1], names(varimp$importance)[2])
      varimp.mod = varimp.mod[ , -4]
      plot.varimp = plot(varimp, main = "Variable Importance", ...)
      x = list("Variable Importance" = varimp.mod, "Variable Importance Plot" = plot.varimp)
      return(x)
    }
  }

  else if(is.numeric(yvec) & predict) {
    varimp = caret::varImp(model, scale = scale, ...)
    varimp.mod = data.frame(rownames(varimp$importance), matrix(unlist(varimp$importance), nrow = nrow(varimp$importance), byrow = TRUE))
    varimp.mod$VariableName = rownames(varimp.mod)
    colnames(varimp.mod) = c("Variable", names(varimp$importance)[1], names(varimp$importance)[2])
    varimp.mod = varimp.mod[ , -4]
    plot.varimp = plot(varimp, main = "Variable Importance", ...)
    pred.mod = caret::predict.train(model, newdata, type = "raw")
    respred.df = data.frame(Residuals = resids, Predicted = pred.mod)
    respred.plot1 = ggplot2::ggplot(respred.df, ggplot2::aes(x = Predicted, y = Residuals))
    respred.plot2 = respred.plot + ggplot2::geom_point(respred.df, ggplot2::aes(x = Predicted, y = Residuals)) + ggplot2::theme_bw() + ggplot2::labs(x = "Predicted", y = "Residuals")
    x = list("Variable Importance" = varimp.mod, "Variable Importance Plot" = plot.varimp, "Predictions" = pred.mod, "Residuals Vs. Predicted Plot" = respred.plot2)
    return(x)
  }

  else if(is.numeric(yvec) & !predict) {
    varimp = caret::varImp(model, scale = scale, ...)
    varimp.mod = data.frame(rownames(varimp$importance), matrix(unlist(varimp$importance), nrow = nrow(varimp$importance), byrow = TRUE))
    varimp.mod$VariableName = rownames(varimp.mod)
    colnames(varimp.mod) = c("Variable", names(varimp$importance)[1], names(varimp$importance)[2])
    varimp.mod = varimp.mod[ , -4]
    plot.varimp = plot(varimp, main = "Variable Importance", ...)
    x = list("Variable Importance" = varimp.mod, "Variable Importance Plot" = plot.varimp)
    return(x)
  }
}

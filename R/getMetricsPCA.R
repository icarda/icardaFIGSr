
#' @title Performance Measures with PCA pre-processing
#' @description getMetricsPCA allows to obtain performance measures from Confusion Matrix for algorithms with PCA pre-processing,it returns a data frame containing performance measures from the confusion matrix given by the \code{caret} package when algorithms have been run with PCA pre-processing.
#' @param y expression. The class variable.
#' @param yhat expression. The vector of predicted values.
#' @param classtype character or numeric. The number of levels in \code{y}.
#' @param model expression. The model object to which output of the model has been assigned.
#' @return Outputs an object with performance measures calculated from the confusion matrix given by the \code{caret} package. A data frame is the resulting output with the first column giving the name of the performance measure, and the second column giving the corresponding value.
#' @details Works with target variables that have two, three, four, six or eight classes. Similar to \code{\link[icardaFIGSr]{getMetrics}} but used in the case where models have been run with PCA specified as an option for the \code{preProcess} argument in the \code{train} function of \code{caret}.
#' @author Khadija Aziz, Zainab Azough, Zakaria Kehel, Bancy Ngatia
#' @examples
#' \dontrun{
#'  # Obtain predictions from several previously run models
#'  dataX <- subset(data, select = -y)
#'  pred.knn <- predict(model.knn, newdata = dataX)
#'  pred.rf <- predict(model.rf, newdata = dataX)
#'
#'  # Get metrics for several algorithms
#'  metrics.knn <- getMetricsPCA(y = data$y, yhat = pred.knn,
#'                               classtype = 2, model = model.knn)
#'  metrics.rf <- getMetricsPCA(y = data$y, yhat = pred.rf,
#'                              classtype = 2, model = model.rf)
#'
#'  # Indexing for 2-class models to remove extra column with
#'  # names of performance measures
#'  metrics.all <- cbind(metrics.knn, metrics.rf[ , 2])
#'
#'  # No indexing needed for 3-, 4-, 6- or 8-class models
#'  metrics.all <- cbind(metrics.knn, metrics.rf)
#'  }
#' @seealso
#'  \code{\link[caret]{confusionMatrix}},
#'  \code{\link[caret]{predict.train}}
#' @name getMetricsPCA
#' @importFrom caret confusionMatrix predict.train
#' @export

getMetricsPCA <- function(yhat, y, classtype, model) {
  cm = caret::confusionMatrix(y, caret::predict.train(model, yhat))
  accu = data.frame(measures = "Accuracy", values = format(data.frame(cm$overall)[1, ], digits = 3))
  lcl = format(data.frame(cm$overall)[3, ], digits = 3)
  ucl = format(data.frame(cm$overall)[4, ], digits = 3)
  ci = data.frame(measures = "95% CI", values = paste0("(", lcl, ", ", ucl, ")"))
  nir = data.frame(measures = "No Information Rate", values = format(data.frame(cm$overall)[5, ], digits = 3))
  pval = data.frame(measures = "P-Value [Acc > NIR]", values = data.frame(cm$overall)[6, ])
  kap = data.frame(measures = "Kappa", values = format(data.frame(cm$overall)[2, ], digits = 3))
  
  if (length(yhat) == length(y)){
    if( classtype == 2) {
      s = data.frame(format(data.frame(cm$byClass)[1:2, 1], digits = 3))
      rownames(s) <- c("Sensitivity", "Specificity")
      colnames(s) <- "1"
    } 
    else {
      
      sensitivity <- as.data.frame(format(cm$byClass[,1],digits = 3))
      rownames(sensitivity) <- paste("Sensitivity",sprintf("Class %s",seq(1:classtype)), sep = " ")
      colnames(sensitivity) <- "Metrics"
      
      specificity <- as.data.frame(format(cm$byClass[,2],digits = 3))
      rownames(specificity) <- paste("Specificity",sprintf("Class %s",seq(1:classtype)),sep = " ")
      colnames(specificity) <- "Metrics"
      
      recall <- as.data.frame(format(cm$byClass[,6],digits = 3))
      rownames(recall) <- paste("Recall",sprintf("Class %s",seq(1:classtype)),sep = " ")
      colnames(recall) <- "Metrics"
      
      balancedAccuracy <- as.data.frame(format(cm$byClass[,11],digits = 3))
      rownames(balancedAccuracy) <- paste("balanced Accuracy",sprintf("Class %s",seq(1:classtype)),sep = " ")
      colnames(balancedAccuracy) <- "Metrics"
      
      s <- rbind(sensitivity,specificity,recall,balancedAccuracy)
    }
    
    table1 = as.data.frame.matrix(cm$table)
    measures = rbind(accu[1], ci[1], nir[1], pval[1], kap[1])
    values = rbind(as.character(as.factor(accu$values)),
                   as.character(as.factor(ci$values)),
                   as.character(as.factor(nir$values)),
                   format(as.numeric(as.character(pval$values), 3)),
                   as.character(as.factor(kap$values)))
    
    metrics.df = as.data.frame(values)
    row.names(metrics.df)<- measures$measures
    colnames(metrics.df) <- "Metrics"
    metrics.df2 = rbind(metrics.df,s)
    metrics.df3 <- NULL
    metrics.df3$Metrics <- metrics.df2
    metrics.df3$CM <- table1
    
    
    return(metrics.df3)
    
  }
}

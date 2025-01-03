#' @title Performance Measures for classification tasks
#' @description this function allows to obtain performance measures from Confusion Matrix, it returns a data frame containing performance measures from the confusion matrix given by the \code{caret} package.
#' @param y expression. The class variable.
#' @param yhat expression. The vector of predicted values.
#' @param classtype character or numeric. The number of levels in \code{y}.
#' @return Outputs an object with performance measures calculated from the confusion matrix given by the \code{caret} package. A data frame is the resulting output with the first column giving the name of the performance measure, and the second column giving the corresponding value.
#' @details \code{getMetrics} works with target variables that have two, three, four, six or eight classes.
#'
#' The function relies on the \code{caret} package to obtain the confusion matrix from which performance measures are extracted. It can be run for several algorithms, and the results combined into one data frame for easier comparison (see section 'Examples').
#'
#' Predictions have to be obtained beforehand and used as input for \code{yhat}. The \code{predict.train} function in \code{caret} should be run without argument \code{type} when obtaining the predictions.
#' @author Zakaria Kehel, Bancy Ngatia, Khadija Aziz
#' @examples
#' \dontrun{
#'# Obtain predictions from previous models
#'  
#' data(septoriaDurumWC)  
#' split.data <- splitData(septoriaDurumWC, seed = 1234, y = "ST_S", p = 0.7)
#' data.train <- split.data$trainset
#' data.test <- split.data$testset
#'
#' knn.mod <- tuneTrain(data = septoriaDurumWC,y = 'ST_S',method = 'knn',positive = 'R')
#' nnet.mod <- tuneTrain(data = septoriaDurumWC,y = 'ST_S',method = 'nnet',positive = 'R') 
#'
#' pred.knn <- predict(knn.mod$Model, newdata = data.test[ , -1])
#' pred.nnet <- predict(nnet.mod$Model, newdata = data.test[ , -1])
#'
#' metrics.knn <- getMetrics(y = data.test$ST_S, yhat = pred.knn, classtype = 2)
#' metrics.nnet <- getMetrics(y = data.test$ST_S, yhat = pred.nnet, classtype = 2)
#' }
#' @seealso
#'  \code{\link[caret]{confusionMatrix}}
#' @name getMetrics
#' @importFrom caret confusionMatrix
#' @export

getMetrics <- function(y, yhat, classtype) {
  cm = caret::confusionMatrix(xtabs(~ yhat + y))
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
    values = rbind(as.character(as.factor(accu$values)), as.character(as.factor(ci$values)), as.character(as.factor(nir$values)), format(as.numeric(as.character(pval$values), 3)), as.character(as.factor(kap$values)))
    metrics.df = as.data.frame(values)
    row.names(metrics.df)<- measures$measures
    colnames(metrics.df) <- "Metrics"
    colnames(s) <- "Metrics"
    metrics.df2 = rbind(metrics.df,s)
    metrics.df3 <- NULL
    metrics.df3$Metrics <- metrics.df2
    metrics.df3$CM <- table1
    
    
    return(metrics.df3)
    
  }
}

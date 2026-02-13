#' @title Automated Data Splitting, Tuning, and Model Training
#' 
#' @description A high-level wrapper for the `caret` framework that automates 
#' data partitioning, two-stage hyperparameter tuning (coarse and refined), 
#' and comprehensive model evaluation for both classification and regression.
#' 
#' @param data A \code{data.frame} containing the target variable and predictors.
#' @param y Character. The name of the target variable column in \code{data}.
#' @param p Numeric. Proportion of data used for training (0 < p < 1). Default: 0.7.
#' @param method Character. The model type (e.g., "knn", "rf", "nnet").
#' Use \code{names(caret::getModelInfo())} for available options.
#' @param parallelComputing Logical. If \code{TRUE}, uses \code{doParallel} for 
#' multi-core processing. Default: \code{FALSE}.
#' @param length Integer. Initial number of tuning parameter combinations. Default: 10.
#' @param control Character. Resampling method (e.g., "cv", "repeatedcv"). Default: "repeatedcv".
#' @param number Integer. Number of folds or resampling iterations. Default: 10.
#' @param repeats Integer. Number of repeats for k-fold cross-validation. Default: 10.
#' @param process Character vector. Pre-processing steps (e.g., "center", "scale").
#' Default: \code{c("center", "scale")}.
#' @param summary Function. Performance metric computation. Default: \code{multiClassSummary}.
#' @param positive Character. The positive class for binary classification.
#' Default: \code{NULL}.
#' @param imbalanceMethod `r lifecycle::badge("experimental")` Character. Subsampling method ("up" or "down"). Default: \code{NULL}.
#' @param ... Additional arguments passed to \code{\link[caret]{trainControl}} and \code{\link[caret]{train}}.
#' 
#' @return #' @return A \code{list} containing:
#' \itemize{
#'   \item \code{Tuning}: Initial model tuning results.
#'   \item \code{Training}: Final model trained on optimized parameters.
#'   \item \code{ModelQuality}: Confusion Matrix (Classification) or Residual Metrics (Regression).
#'   \item \code{VariableImportance}: A plot of relative feature importance.
#'   \item \code{Predictions}: Predicted values for the test set.
#'   \item \code{ROC_Plot}: ROC plots (Classification).
#'   \item \code{Residuals vs. Predicted Plot}: Residual analysis (Regression).
#'   \item \code{Predicted vs. Actual Plot}: Actual vs Predicted values (Regression).
#'   \item \code{Training Data} / \code{Test Data}: The partitioned datasets.
#' }
#' 
#' @details
#' The function automatically detects the task type:
#' \itemize{
#'   \item If \code{y} is a \strong{factor}, classification is performed with class probability estimation.
#'   \item If \code{y} is \strong{numeric}, regression is performed.
#' }
#' A two-stage tuning process is used for specific methods (KNN, RF, SVM, NNET) where 
#' an initial coarse search is followed by a refined grid search centered on the best initial parameters.
#''
#' @author Chafik Analy, Khadija Aziz, Zakaria Kehel, Bancy Ngatia, Khadija Aouzal
#' @examples
#' \dontrun{
#' # Loading test datasets
#' data(DurumWheatDHEWC)
#' data(BarleyRNOWC)
#' data(septoriaDurumWC)
#' 
#' ## Binary classification of ST_S with balanced data
#' tree.ST_S <- tuneTrain(data = as.data.frame(septoriaDurumWC),
#'                      y =  'ST_S',
#'                      method = 'treebag',
#'                      summary = multiClassSummary,
#'                      repeats = 3, positive = 'R')
#' 
#' ## Binary classification of RNO with imbalanced data
#' knn.RNO <- tuneTrain(data = BarleyRNOWC,
#'                      y =  'RNO',
#'                      method = 'knn',
#'                      summary = multiClassSummary,
#'                      process = c("scale","center"),
#'                      imbalanceMethod ="up",
#'                      repeats = 3)
#'                       
#' ## Regression of DHE
#' svm.DHE <- tuneTrain(data = DurumWheatDHEWC,
#'                      y =  'DHE',
#'                      method = 'svmLinear2',
#'                      summary = defaultSummary,
#'                      repeats = 3)
#'  }
#' @seealso
#'  \code{\link[caret]{trainControl}},
#'  \code{\link[caret]{train}},
#'  \code{\link[caret]{predict.train}}
#' @name tuneTrain
#' @importFrom caret createDataPartition trainControl train predict.train confusionMatrix varImp postResample multiClassSummary
#' @importFrom utils stack
#' @importFrom ggplot2 ggplot aes geom_histogram theme_bw scale_colour_brewer scale_fill_brewer labs geom_point geom_line geom_abline facet_wrap ggtitle coord_equal geom_smooth
#' @importFrom stats predict
#' @importFrom pROC auc roc ggroc multiclass.roc
#' @export

tuneTrain <- function(data, y, p = 0.7,
                      method = "knn",
                      parallelComputing = FALSE,
                      length = 10,
                      control = "repeatedcv",
                      number = 10, repeats = 10,
                      process = c("center", "scale"),
                      summary = multiClassSummary,
                      positive = NULL,
                      imbalanceMethod = NULL, ...) {

  # Ensure target variable exists in the dataset
  if (!y %in% names(data)) stop("Error: Target variable '", y, "' not found in the dataset.")

  # Level Sanitization and Mapping
  if (is.factor(data[[y]])) {
    orig_levels <- levels(data[[y]])
    valid_levels <- make.names(orig_levels)
    name_map <- setNames(orig_levels, valid_levels)
    
    # Update the data to valid names for caret
    levels(data[[y]]) <- valid_levels
    
    # Positive Class Handling
    if (length(valid_levels) == 2) {
      if (is.null(positive)) {
        # Default to the first alphabetical original level
        display_positive <- orig_levels[1]
        positive <- valid_levels[1]
        message("Positive class not defined. Defaulting to the first alphabetical class: '", display_positive, "'.")
      } else {
        # Store original for plots, sanitize for logic
        display_positive <- as.character(positive)
        positive <- make.names(display_positive)
        
        if (!(positive %in% valid_levels)) {
          stop("Provided positive class '", display_positive, "' not found in levels: ", 
               paste(orig_levels, collapse = ", "))
        }
      }
    }
  }

  # Parallel computing initialization if enabled
  if (parallelComputing) {
    closeAllConnections()
    cores <- parallel::detectCores()
    cls <-  parallel::makeCluster(max(1, cores - 1))
    doParallel::registerDoParallel(cls)
    on.exit({if (exists("cls")) parallel::stopCluster(cls)
      foreach::registerDoSEQ()
      }, add = TRUE)
  }
  
  # Set a seed for reproducibility
  set.seed(1234)
  
  # Determine if classification or regression
  if (is.factor(data[[y]])) {
    subsampling <- imbalanceMethod
    classProbs <- TRUE
  }
  else {
    subsampling <- NULL
    classProbs <- FALSE
  }

  # Data splitting
  trainIndex <- caret::createDataPartition(y = data[[y]], p = p, list = FALSE)
  data.train <- data[trainIndex, ]
  data.test <- data[-trainIndex, ]

  trainx <- data.train[, names(data.train) != y, drop = FALSE]
  trainy <- data.train[[y]]
  testx <- data.test[, names(data.test) != y, drop = FALSE]
  testy <- data.test[[y]]

  # Control object for training
  ctrl <- caret::trainControl(method = control,
                        number = number,
                        repeats = repeats,
                        sampling = subsampling,
                        summaryFunction = summary,
                        classProbs = classProbs, ...)
  
  # Coarse tuning
  tune.mod = caret::train(trainx, trainy, method = method,
                          tuneLength = length, preProcess = process, trControl = ctrl, ...)
  
  # Refined tuning
  grid <- .generate_refined_grid(method, tune.mod, trainx)
  if (!is.null(grid)) {
    message("Refined Training with optimized grid...")
    train.mod <- caret::train(trainx, trainy, method = method, 
                              tuneGrid = grid, tuneLength = length,
                              preProcess = process, trControl = ctrl, ...)
  } else {
    train.mod <- tune.mod
  }
  
  # Evaluating the model and getting results
  return(.evaluate_model(train.mod, tune.mod, testx, testy, data.train, 
                         data.test, positive, display_positive, name_map))
  
}

# Internal helper for generating the refined grid
.generate_refined_grid <- function(method, tune_mod, trainx) {
  best <- tune_mod$bestTune
  switch(method,
         "nnet" = {
           max_size <- min(ncol(trainx), floor(nrow(trainx) / 10))
           expand.grid(.size = seq(max(1, best$size - 1), min(max_size, best$size + 1), by = 1),
                       .decay = 10^seq(-5, -1, length.out = 5))
         },
         "knn"  = {
           seqStart <- if (best$k - 2 <= 0) best$k else best$k - 2
           expand.grid(.k = seq(seqStart, best$k + 2, by = 1))
         },
         "rf"   = expand.grid(.mtry = seq(max(1, best$mtry - 2), min(ncol(trainx), best$mtry + 2), by = 1)),
         "svmLinear2" = {
           seqStart <- if (best$cost <= 1) best$cost else best$cost - 1
           expand.grid(.cost = seq(seqStart, best$cost + 1, by = 0.25))
         },
         NULL # Default for treebag or others
  )
}

# Internal helper for evaluating the model
.evaluate_model <- function(train_mod, tune_mod, testx, testy, 
                            d_train, d_test, positive = NULL, display_positive = NULL, name_map = NULL) {
  preds <- stats::predict(train_mod, testx)
  # Common components
  res <- list(Tuning = tune_mod, Training = train_mod, 
              `Training Data` = d_train, `Test Data` = d_test,
              Predictions = preds,
              VariableImportance = plot(caret::varImp(train_mod)))
  
  if (is.factor(testy)) {
    # Classification task
    probs <- stats::predict(train_mod, testx, type = "prob")
    res$ModelQuality <- caret::confusionMatrix(preds, testy)
    res$ProbabilitiesPlot <- .plot_class_probs(probs, name_map)
    
    # ROC Logic
    if (nlevels(testy) == 2) {
      # Identify the negative class
      all_levels <- levels(testy)
      negative_class <- setdiff(all_levels, positive)

      roc_obj <- pROC::roc(response = testy, 
                          predictor = probs[[positive]], 
                          levels = c(negative_class, positive),
                          direction = "<",
                          quiet = TRUE)
      
      auc_val <- round(pROC::auc(roc_obj), 4)
      
      roc_plot_untitled <- pROC::ggroc(roc_obj) + 
        ggplot2::theme_bw() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                          label = paste("AUC =", auc_val)) 
      
      if(display_positive != positive)
        res$ROC_Plot <- roc_plot_untitled +
          ggplot2::labs(title = paste("ROC Curve for Class", display_positive),
                        subtitle = paste("Model-internal name:", positive))
      else
        res$ROC_Plot <- roc_plot_untitled + 
          ggplot2::labs(title = paste("ROC Curve for Class", display_positive))
        
    } else {
      # Multiclass ROC
      res$AUC_Values <- sapply(colnames(probs), function(cl) {
        pROC::auc(pROC::roc(ifelse(testy == cl, 1, 0), probs[,cl], quiet = TRUE))
      })

      #multiclass ROC plot
      mc_roc <- pROC::multiclass.roc(testy, probs, quiet = TRUE)
      roc_list <- mc_roc$rocs
      class_names <- mc_roc$levels
      
      # convert to a dataframe
      df_list <- lapply(seq_along(roc_list), function(i) {
        r <- roc_list[[i]]
        if(is.list(r) & !inherits(r, "roc")) r <- r[[1]] 
        
        data.frame(
          fpr = 1 - r$specificities, # false positive rate
          tpr = r$sensitivities, # true positive rate
          Curve = name_map[[class_names[i]]]
        )
      })
      
      roc_data <- do.call(rbind, df_list)
      
      # Combine plots with facet_wrap
      res$ROC_Plot <- ggplot2::ggplot(roc_data, ggplot2::aes(x = fpr, y = tpr)) +
        ggplot2::geom_line(ggplot2::aes(color = Curve)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
        ggplot2::facet_wrap(~ Curve) + 
        ggplot2::labs(
          title = "Multiclass ROC Curves",
          x = "1 - Specificity (False Positive Rate)",
          y = "Sensitivity (True Positive Rate)"
        ) +
        ggplot2::theme_bw() +
        ggplot2::coord_equal()
        
    }
  } else {
    # Regression task
    res$QualityMetrics <- caret::postResample(preds, testy)

    # Data frame for plotting
    plot_df <- data.frame(Actual = as.numeric(testy), Predicted = as.numeric(preds))
    plot_df$Residuals <- plot_df$Actual - plot_df$Predicted

    # Residuals vs Predicted
    res$`Residuals vs. Predicted Plot` <- ggplot2::ggplot(data = plot_df, ggplot2::aes(x = Predicted, y = Residuals)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Residual Analysis", x = "Predicted", y = "Residuals")
    
    # Predicted vs Actual
    res$`Predicted vs. Actual Plot` <- ggplot2::ggplot(data = plot_df, ggplot2::aes(x = Actual, y = Predicted)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Actual", y = "Predicted", title = "Predicted vs Actual Values")
  }
  return(res)
}

# Internal helper for plotting class probabilities
.plot_class_probs <- function(probs, name_map) {
  df <- utils::stack(probs)
  colnames(df) <- c("Probability", "Class")
  
  df$Class <- factor(df$Class, 
                     levels = names(name_map), 
                     labels = as.character(name_map))
  
  ggplot2::ggplot(df, ggplot2::aes(x = Probability, fill = Class, color = Class)) +
    ggplot2::geom_histogram(alpha = 0.4, position = "identity", binwidth = 0.05) +
    ggplot2::theme_bw() +
    ggplot2::scale_colour_brewer(palette = "Dark2") +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::facet_wrap(~ Class) +
    ggplot2::labs(y = "Count")
}
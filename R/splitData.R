
#' @title Splitting Data
#' @description this function splits the Data into Train and Test Sets, it returns a list containing two data frames for the train and test sets.
#' @param data object of class "data.frame" with target variable and predictor variables.
#' @param seed integer. Values for the random number generator. Default: NULL.
#' @param y character. Target variable.
#' @param p numeric. Proportion of data to be used for training.
#' @param ... additional arguments to be passed to \code{createDataPartition} function in \code{caret} package to control the way the data is split.
#' @return A list with two data frames: the first as train set, and the second as test set.
#' @details \code{splitData} relies on the \code{createDataPartition} function from the \code{caret} package to perform the data split.
#'
#' If \code{y} is a factor, the sampling of observations for each set is done within the levels of \code{y} such that the class distributions are more or less balanced for each set.
#'
#' If \code{y} is numeric, the data is split into groups based on percentiles and the sampling done within these subgroups. See \code{\link[caret]{createDataPartition}} for more details on additional arguments that can be passed.
#' @author Zakaria Kehel, Bancy Ngatia
#' @examples
#' \dontrun{
#'  # Split the data into 70/30 train and test sets for factor y
#'  data(septoriaDurumWC)
#'  split.data <- splitData(septoriaDurumWC, seed = 1234,
#'                          y = 'ST_S', p = 0.7)
#'
#'  # Get training and test sets from list object returned
#'  trainset <- split.data$trainset
#'  testset <- split.data$testset
#' }
#' @seealso
#'  \code{\link[caret]{createDataPartition}}
#' @name splitData
#' @importFrom caret createDataPartition
#' @export

splitData <- function(data, seed = NULL, y, p, ...) {

  if(!is.null(seed)) set.seed(seed)

  yvec = data[[y]]
  trainIndex = caret::createDataPartition(y = yvec, p = p, list = FALSE, ...)
  data.train = as.data.frame(data[trainIndex, ])
  data.test = as.data.frame(data[-trainIndex, ])
  list(trainset = data.train, testset = data.test)
}

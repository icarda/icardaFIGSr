#' @title Crops Available in ICARDA's Genebank
#' @description Returns a list of crops available in ICARDA's Genebank Database.
#' @return A list containing all crops available in ICARDA's Genebank Database.
#' @author Zakaria Kehel, Fawzy Nawar, Khadija Aouzal
#' @examples
#' \dontrun{
#'  # Get list of available crops
#'  crops <- getCrops()
#'  }
#' @name getCrops
#' @importFrom utils read.csv
#' @export

getCrops <- function() {
  result <- read.csv("http://grs.icarda.org/web_services/getCrops.php")
  df <- data.frame(colnames(result)[1],colnames(result)[2])
  names(df) <- c("CropCode","CropName")
  names(result)<- c("CropCode","CropName")
  crops <- rbind(df,result)
  return(crops)
}

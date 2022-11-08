
#' @title Crops Available in ICARDA's Genebank
#' @description this function allows to obtain a list of crops available in ICARDA's Genebank Documentation System, it returns a list with codes and names of available crops.
#' @return A list containing all crops available in ICARDA's Genebank Documentation System.
#' @details The crop codes and names are fetched from ICARDA's online server.
#' @author Zakaria Kehel, Fawzy Nawar 
#' @examples
#' if(interactive()){
#'  # Get list of available crops
#'  crops <- getCrops()
#'  }
#' @rdname getCrops
#' @export
#' @importFrom utils read.csv

getCrops <- function() {
  result <- read.csv("http://grs.icarda.org/web_services/getCrops.php")
  df <- data.frame(colnames(result)[1],colnames(result)[2])
  names(df) <- c("CropCode","CropName")
  names(result)<- c("CropCode","CropName")
  crops <- rbind(df,result)
  return(crops)
}

# utils::globalVariables(c("climate.df", "cumgdd", "Probability", "Predicted",
#                          "Residuals", "addHandlerClicked", "climate.cv.df", "dispose",
#                          "gbutton", "gedit", "ggroup", "glabel", "multiClassSummary",
#                          "resids", "respred.plot", "svalue", ".credentials",
#                          "Class", "ICBW_Onset_Phen", "ICB_Onset_Phen", "ICDW_Onset_Phen",
#                          "IFPI_Onset_Phen", "ILB_Onset_Phen", "ILC_Onset_Phen", "ILL_Onset_Phen",
#                          "value", "onset", "Cycle", "DAP", "tmin", "tmax","Variable", "Importance",
#                          "visible","visible<-"))

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      # tuneTrain Variables
      "fpr", "tpr", "Curve", "Probability", "Class",
      "Actual", "Predicted", "Residuals",
      
      # getDaily & getOnset Variables
      "climate.df", "climate.cv.df",
      
      # getGrowthPeriod Variables
      "value", "cumgdd", "onset", "Cycle", "DAP", 
      "tmin", "tmax", "stage",
      
      # getOnset Phenology Variables
      "ICDW_Onset_Phen", "ICB_Onset_Phen", "ICBW_Onset_Phen",
      "ILL_Onset_Phen", "ILB_Onset_Phen", "ILC_Onset_Phen", 
      "IFPI_Onset_Phen",
      
      # varimpPred Variables
      "Variable", "Importance"
    )
  )
}

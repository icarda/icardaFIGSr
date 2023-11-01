
#' @title Calculating Growing Degree Days and Lengths of Growth Stages for Various Crops Using Onset Data from ICARDA's Database
#' @description Calculates growing degree days (GDD) as well as cumulative GDD, and returns a list of various data frames based on specified arguments.
#' @param sitecode expression. Vector with names of sites from which to extract onset data.
#' @param crop character. Type of crop in ICARDA database. See section 'Details' for crops which have calculations available.
#' @param base integer. Minimum temperature constraint for the crop.
#' @param max integer. Maximum temperature constraint for the crop.
#' @param gdd boolean. If \code{TRUE}, returns a data frame containing calculated GDD and accumulated GDD together with climatic variables used for the calculations. Default: FALSE.
#' @return A list object with different data frames depending on specified option in \code{gdd}.
#' If \code{gdd = TRUE}, the object is a list containing three data frames: the first one with lengths of different growing stages, the second one with original onset data with phenological variables, and the third one with calculated GDD and accumulated GDD for the sites specified in \code{sitecode}.
#' If \code{gdd = FALSE}, the object is a list containing two data frames: the first one with lengths of different growing stages, and the second one with original onset data with phenological variables for the sites specified in \code{sitecode}.
#' @details Growing degree days for various crops are calculated using average daily minimum and maximum temperature values obtained from onset data. The temperature constraints specified in \code{base} and \code{max} are first applied before the calculations are done. These constraints ensure very low or high temperatures which prevent growth of a particular crop are not included.
#' Crops for which GDD calculations are available include: 'Durum wheat', 'Bread wheat', 'Barley', 'Chickpea', 'Lentil'. Each of these can be supplied as options for the argument \code{crop}.
#' Cumulative GDD values determine the length of different growing stages. Growing stages vary depending on the type of crop. Durum wheat, bread wheat and barley have five growth stages, i.e. beginning of heading, beginning and completion of flowering, and beginning and completion of grain filling. Chickpea and lentil have four growth stages, i.e. beginning of flowering, completion of 50% flowering, beginning of seed filling, and completion of 90% maturity (chickpea) or of full maturity (lentil).
#' The length of the full growth cycle of the crop for each site is also given in the output data frame.
#' @author Khadija Aouzal, Zakaria Kehel, Bancy Ngatia
#' @examples
#' if(interactive()){
#'  # Calculate GDD for durum wheat 
#'  data(durumDaily)
#'  
#'  durumDailysubset 
#'  
#'  growth <- getGrowthPeriod(sitecode = unique(durumDaily$site_code)[1:3],
#'                            crop = 'Durum wheat', base = 0,
#'                            max = 35, gdd = TRUE)
#'
#'  # Get dataframe with lengths of growth stages from list
#'  # object returned
#'  growth.lengths <- growth[[1]]
#'
#'  # Get dataframe with phenotypic variables from list
#'  # object returned
#'  growth.pheno <- growth[[2]]
#'
#'  # Get dataframe with GDD, cumulative GDD and climatic
#'  # variables from list object returned (when gdd = TRUE)
#'  growth.gdd <- growth[[3]]
#'  }
#'  
#' @rdname getGrowthPeriod
#' @importFrom dplyr group_by slice
#' @importFrom magrittr "%>%"
#' @importFrom plyr ddply join
#' @importFrom reshape2 melt
#' @export


getGrowthPeriod <- function(sitecode, crop, base, max, gdd = FALSE) {
  
  # Map full crop names to crop codes
  if(crop == "Durum wheat") onsetcrop = "ICDW"
  else if(crop == "Bread wheat") onsetcrop = "ICBW"
  else if(crop == "Barley") onsetcrop = "ICB"
  else if(crop == "Chickpea") onsetcrop = "ILC"
  else if(crop == "Lentil") onsetcrop = "ILL"
  
  # Fetch the onset data based on the site and crop code
  onsetdata = getOnset(sites = levels(as.factor(sitecode)), var = c('tmin', 'tmax'), 
                       crop = onsetcrop)
  
  # Separate temperature and phenological data
  tempdata = onsetdata[[1]]
  phenodata = onsetdata[[2]]
  
  # Rename columns for consistency
  names(tempdata)[names(tempdata) == grep("site", colnames(tempdata), value = TRUE)] <- "sitecode"
  
  # More column renaming
  names(phenodata)[names(phenodata) == grep("site", colnames(phenodata), value = TRUE)] <- "sitecode"
  names(phenodata)[names(phenodata) == grep("onset", colnames(phenodata), value = TRUE)] <- "onset"
  
  # Identify temperature columns in the data
  tminvars = grep("tmin", colnames(tempdata), perl = TRUE, value = TRUE)
  tmaxvars = grep("tmax", colnames(tempdata), perl = TRUE, value = TRUE)
  
  # Reshape the temperature data for further processing
  data2 = reshape2::melt(tempdata, id = "sitecode")
  data2 = data2[order(data2[ , "sitecode"]), ]
  
  # Replace temperatures below the base with the base value
  data2[3] = lapply(data2[3], function(x) replace(x, x < base, base))
  
  # Calculate growing degree days (GDD)
  gddtmin = data2[data2$variable %in% tminvars, "value"]
  gddtmax = data2[data2$variable %in% tmaxvars, "value"]
  gddvar = (gddtmin + gddtmax) / 2
  
  # Create a new data frame 'data4' containing 'sitecode', 'tmin', 'tmax', and 'gdd' based on conditions in 'data2' and other variables
  data4 = data.frame(sitecode = data2[data2$variable %in% tminvars, "sitecode"], tmin = gddtmin, tmax = gddtmax, gdd = gddvar)
  
  # Group 'data4' by 'sitecode' and calculate the cumulative sum of 'gdd' for each group, storing it in 'cumgdd'
  data4 = plyr::ddply(data4, plyr::.(sitecode), transform, cumgdd = cumsum(gdd))
  
  # Add a new column 'DAP' (Days After Planting) to 'data4', filled with a sequence from 1 to 365
  data4 = plyr::ddply(data4, plyr::.(sitecode), transform, DAP = seq(1, 365, 1))
  
  # Join 'data4' with 'phenodata' on 'sitecode' and 'onset', and suppress warning messages
  data5 = suppressMessages(plyr::join(data4, phenodata[ , c("sitecode", "onset")]))
  
  # Conditional logic for handling 'Durum wheat' and 'Bread wheat' crops
  if(crop %in% c("Durum wheat", "Bread wheat")) {
    
    # Modify 'tmax' values based on the conditions involving 'cumgdd'
    data5 = within(data5, tmax[tmax > 21 & cumgdd <= 395] <- 21)
    data5 = within(data5, tmax[tmax > max & cumgdd > 395] <- max)
    
    # For each of the growth stages ('HB', 'FB', etc.), find the row with the nearest 'cumgdd' to specific values
    # Calculate new dates for each growth stage, accounting for possible year rollover
    
    # Heading and Booting stage (HB)
    HB.df = data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1567)))
    HB.df$HB = ifelse((HB.df$DAP + HB.df$onset) > 365, (HB.df$DAP + HB.df$onset) - 365, HB.df$DAP + HB.df$onset)
    
    # Subset columns for HB stage
    HB.df2 <- HB.df[ , c("sitecode", "onset", "HB")]
    
    # Flowering and Booting stage (FB)
    FB.df = data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1739)))
    FB.df$FB = ifelse((FB.df$DAP + FB.df$onset) > 365, (FB.df$DAP + FB.df$onset) - 365, FB.df$DAP + FB.df$onset)
    
    # Subset columns for FB stage
    FB.df2 <- FB.df[ , c("sitecode", "onset", "FB")]
    
    # Fully Covered stage (FC)
    FC.df = data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1768)))
    FC.df$FC = ifelse((FC.df$DAP + FC.df$onset) > 365, (FC.df$DAP + FC.df$onset) - 365, FC.df$DAP + FC.df$onset)
    
    # Subset columns for FC stage
    FC.df2 <- FC.df[ , c("sitecode", "onset", "FC")]
    
    # Growth From Booting (GFB) stage
    GFB.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1825)))
    GFB.df$GFB = ifelse((GFB.df$DAP + GFB.df$onset) > 365, (GFB.df$DAP + GFB.df$onset) - 365, GFB.df$DAP + GFB.df$onset)
    
    # Subset columns for GFB stage
    GFB.df2 <- GFB.df[ , c("sitecode", "onset", "GFB")]
    
    # Growth Fully Complete (GFC) stage
    GFC.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 2170)))
    GFC.df$GFC = ifelse((GFC.df$DAP + GFC.df$onset) > 365, (GFC.df$DAP + GFC.df$onset) - 365, GFC.df$DAP + GFC.df$onset)
    
    # Subset columns for GFC stage
    GFC.df2 <- GFC.df[ , c("sitecode", "onset", "GFC")]
    
  }
  
  else if(crop == "Barley") {
    
    # Modify 'tmax' values based on the conditions involving 'cumgdd'
    data5 = within(data5, tmax[tmax > 21 & cumgdd <= 395] <- 21)
    data5 = within(data5, tmax[tmax > max & cumgdd > 395] <- max)
    
    # Heading and Booting stage (HB)
    HB.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1357)))
    HB.df$HB = ifelse((HB.df$DAP + HB.df$onset) > 365, (HB.df$DAP + HB.df$onset) - 365, HB.df$DAP + HB.df$onset)
    
    # Subset columns of HB
    HB.df2 <- HB.df[ , c("sitecode", "onset", "HB")]
    
    # Flowering and Booting (FB)
    FB.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1426)))
    FB.df$FB = ifelse((FB.df$DAP + FB.df$onset) > 365, (FB.df$DAP + FB.df$onset) - 365, FB.df$DAP + FB.df$onset)
    
    # Subset columns of FB
    FB.df2 <- FB.df[ , c("sitecode", "onset", "FB")]
    
    # Fully covered stage (FC) 
    FC.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1496)))
    FC.df$FC = ifelse((FC.df$DAP + FC.df$onset) > 365, (FC.df$DAP + FC.df$onset) - 365, FC.df$DAP + FC.df$onset)
    
    # Subset columns of FC
    FC.df2 <- FC.df[ , c("sitecode", "onset", "FC")]
    
    # Growth From Booting (GFB)
    GFB.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1635)))
    GFB.df$GFB = ifelse((GFB.df$DAP + GFB.df$onset) > 365, (GFB.df$DAP + GFB.df$onset) - 365, GFB.df$DAP + GFB.df$onset)
    
    GFB.df2 <- GFB.df[ , c("sitecode", "onset", "GFB")]
    
    # Growth Fully complete (GFC)
    GFC.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1960)))
    GFC.df$GFC = ifelse((GFC.df$DAP + GFC.df$onset) > 365, (GFC.df$DAP + GFC.df$onset) - 365, GFC.df$DAP + GFC.df$onset)
    
    GFC.df2 <- GFC.df[ , c("sitecode", "onset", "GFC")]
    
  }
  
  # Same logic apply to chickpea
  
  else if(crop == "Chickpea") {
    
    data5 = within(data5, tmax[tmax > max] <- max)
    
    FB.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 680)))
    FB.df$FB = ifelse((FB.df$DAP + FB.df$onset) > 365, (FB.df$DAP + FB.df$onset) - 365, FB.df$DAP + FB.df$onset)
    
    FB.df2 <- FB.df[ , c("sitecode", "onset", "FB")]
    
    FC.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 870)))
    FC.df$FC = ifelse((FC.df$DAP + FC.df$onset) > 365, (FC.df$DAP + FC.df$onset) - 365, FC.df$DAP + FC.df$onset)
    
    FC.df2 <- FC.df[ , c("sitecode", "onset", "FC")]
    
    GFB.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1075)))
    GFB.df$GFB = ifelse((GFB.df$DAP + GFB.df$onset) > 365, (GFB.df$DAP + GFB.df$onset) - 365, GFB.df$DAP + GFB.df$onset)
    
    GFB.df2 <- GFB.df[ , c("sitecode", "onset", "GFB")]
    
    GFC.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1740)))
    GFC.df$GFC = ifelse((GFC.df$DAP + GFC.df$onset) > 365, (GFC.df$DAP + GFC.df$onset) - 365, GFC.df$DAP + GFC.df$onset)
    
    GFC.df2 <- GFC.df[ , c("sitecode", "onset", "GFC")]
  }
  
  # Same logic apply to Lentil with different some stages
  
  else if(crop == "Lentil") {
    
    data5 = within(data5, tmax[tmax > max] <- max)
    
    FB.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 807)))
    FB.df$FB = ifelse((FB.df$DAP + FB.df$onset) > 365, (FB.df$DAP + FB.df$onset) - 365, FB.df$DAP + FB.df$onset)
    
    FB.df2 <- FB.df[ , c("sitecode", "onset", "FB")]
    
    FC.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 980)))
    FC.df$FC50 = ifelse((FC.df$DAP + FC.df$onset) > 365, (FC.df$DAP + FC.df$onset) - 365, FC.df$DAP + FC.df$onset)
    
    FC.df2 <- FC.df[ , c("sitecode", "onset", "FC50")]
    
    SF.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1187)))
    SF.df$SF = ifelse((SF.df$DAP + SF.df$onset) > 365, (SF.df$DAP + SF.df$onset) - 365, SF.df$DAP + SF.df$onset)
    
    SF.df2 <- SF.df[ , c("sitecode", "onset", "SF")]
    
    M.df <- data5 %>% dplyr::group_by(sitecode) %>% dplyr::slice(which.min(abs(cumgdd - 1808)))
    M.df$M = ifelse((M.df$DAP + M.df$onset) > 365, (M.df$DAP + M.df$onset) - 365, M.df$DAP + M.df$onset)
    
    M.df2 <- M.df[ , c("sitecode", "onset", "M")]
  }
  
  # Same logic apply to chickpea
  
  if(crop %in% c("Durum wheat", "Bread wheat", "Barley")) {
    
    # Merge multiple data frames into one
    pheno.df = Reduce(function(...) merge(..., all = TRUE), list(HB.df2, FB.df2, FC.df2, GFB.df2, GFC.df2))
    
    # Compute phenological variables, accounting for year-round cycles
    # Adjusts for year-round cycles by adding 365 if the calculated length is negative
    
    pheno.df$LHB = ifelse((pheno.df$HB - pheno.df$onset) < 0, (pheno.df$HB - pheno.df$onset) + 365, pheno.df$HB - pheno.df$onset)
    pheno.df$LFB = ifelse((pheno.df$FB - pheno.df$HB) < 0, (pheno.df$FB - pheno.df$HB) + 365, pheno.df$FB - pheno.df$HB)
    pheno.df$LFC = ifelse((pheno.df$FC - pheno.df$FB) < 0, (pheno.df$FC - pheno.df$FB) + 365, pheno.df$FC - pheno.df$FB)
    pheno.df$LGFB = ifelse((pheno.df$GFB - pheno.df$FC) < 0, (pheno.df$GFB - pheno.df$FC) + 365, pheno.df$GFB - pheno.df$FC)
    pheno.df$LGFC = ifelse((pheno.df$GFC - pheno.df$GFB) < 0, (pheno.df$GFC - pheno.df$GFB) + 365, pheno.df$GFC - pheno.df$GFB)
    
    # Keep only necessary columns
    data6 = pheno.df[ , c("sitecode", "onset", "LHB", "LFB", "LFC", "LGFB", "LGFC")]
    # Calculate the total growing cycle length for each site
    data6$Cycle = rowSums(data6[ , -c(1:2)])
    
    data5 = suppressMessages(plyr::join(data5, data6[ , c("sitecode", "Cycle")]))
    dataCycle = data5  %>% dplyr::filter(data5$DAP<=data5$Cycle)
    
    data7 = dataCycle[ , c("sitecode", "tmin", "tmax", "gdd", "cumgdd", "DAP")]
    
    # Gather Growth and onset data 
    list.df1 = list(Growth_Period = data6, Onset_Data = phenodata)
    
    # Gather Growth, onset and GDD data
    list.df2 = list(Growth_Period = data6, Onset_Data = phenodata, Growing_Degree_Days = data7)
  }
  
  # Same logic apply to Chickpea
  
  else if(crop == "Chickpea"){
    
    pheno.df = Reduce(function(...) merge(..., all = TRUE), list(FB.df2, FC.df2, GFB.df2, GFC.df2))
    
    pheno.df$LFB = ifelse((pheno.df$FB - pheno.df$onset) < 0, (pheno.df$FB - pheno.df$onset) + 365, pheno.df$FB - pheno.df$onset)
    pheno.df$LFC = ifelse((pheno.df$FC - pheno.df$FB) < 0, (pheno.df$FC - pheno.df$FB) + 365, pheno.df$FC - pheno.df$FB)
    pheno.df$LGFB = ifelse((pheno.df$GFB - pheno.df$FC) < 0, (pheno.df$GFB - pheno.df$FC) + 365, pheno.df$GFB - pheno.df$FC)
    pheno.df$LGFC = ifelse((pheno.df$GFC - pheno.df$GFB) < 0, (pheno.df$GFC - pheno.df$GFB) + 365, pheno.df$GFC - pheno.df$GFB)
    
    data6 = pheno.df[ , c("sitecode", "onset", "LFB", "LFC", "LGFB", "LGFC")]
    data6$Cycle = rowSums(data6[ , -c(1:2)])
    
    data5 = suppressMessages(plyr::join(data5, data6[ , c("sitecode", "Cycle")]))
    dataCycle = data5  %>% dplyr::filter(data5$DAP<=data5$Cycle)
    data7 = dataCycle[ , c("sitecode", "tmin", "tmax", "gdd", "cumgdd", "DAP")]
    
    list.df1 = list(Growth_Period = data6, Onset_Data = phenodata)
    
    list.df2 = list(Growth_Period = data6, Onset_Data = phenodata, Growing_Degree_Days = data7)
  }
  
  # Same logic apply to Lentil
  
  else if(crop == "Lentil"){
    
    pheno.df = Reduce(function(...) merge(..., all = TRUE), list(FB.df2, FC.df2, SF.df2, M.df2))
    
    pheno.df$LFB = ifelse((pheno.df$FB - pheno.df$onset) < 0, (pheno.df$FB - pheno.df$onset) + 365, pheno.df$FB - pheno.df$onset)
    pheno.df$LFC = ifelse((pheno.df$FC50 - pheno.df$FB) < 0, (pheno.df$FC50 - pheno.df$FB) + 365, pheno.df$FC50 - pheno.df$FB)
    pheno.df$LSF = ifelse((pheno.df$SF - pheno.df$FC) < 0, (pheno.df$SF - pheno.df$FC) + 365, pheno.df$SF - pheno.df$FC)
    pheno.df$LM = ifelse((pheno.df$M - pheno.df$SF) < 0, (pheno.df$M - pheno.df$SF) + 365, pheno.df$M - pheno.df$SF)
    
    data6 = pheno.df[ , c("sitecode", "onset", "LFB", "LFC", "LSF", "LM")]
    data6$Cycle = rowSums(data6[ , -c(1:2)])
    
    data5 = suppressMessages(plyr::join(data5, data6[ , c("sitecode", "Cycle")]))
    dataCycle = data5  %>% dplyr::filter(data5$DAP<=data5$Cycle)
    data7 = dataCycle[ , c("sitecode", "tmin", "tmax", "gdd", "cumgdd", "DAP")]
    
    list.df1 = list(Growth_Period = data6, Onset_Data = phenodata)
    
    list.df2 = list(Growth_Period = data6, Onset_Data = phenodata, Growing_Degree_Days = data7)
  }
  
  # Return the appropriate list of data frames based on whether 'gdd' is TRUE or FALSE
  
  if(gdd) return(list.df2)
  
  else if(!gdd) return(list.df1)
}

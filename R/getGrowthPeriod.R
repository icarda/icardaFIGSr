
#' @title Calculating Growing Degree Days and Lengths of Growth Stages for Various Crops
#'  Using Onset Data from ICARDA's Genebank Database
#' @description Calculates growing degree days (GDD) as well as cumulative GDD,
#'  and returns a list of various data frames based on specified arguments.
#' @param sitecode Vector with names of sites from which to extract onset data.
#' @param crop Crop name. Calculations are available for 'Barley', 'Bread wheat',
#'  'Chickpea', 'Durum wheat' and 'Lentil'.
#' @param base Minimum temperature constraint for the crop.
#' @param max  Maximum temperature constraint for the crop.
#' @param gdd If \code{TRUE}, returns a data frame containing calculated GDD
#'  and accumulated GDD together with climatic variables used for the calculations.
#'  Default: FALSE.
#' @return A list object with different data frames depending on specified option in \code{gdd}.
#' If \code{gdd = TRUE}, the object is a list containing three data frames:
#'  the first one with lengths of different growing stages,
#'  the second one with original onset data with phenological variables,
#'  and the third one with calculated GDD and accumulated GDD for the sites specified in \code{sitecode}.
#' If \code{gdd = FALSE}, the object is a list containing two data frames:
#'  the first one with lengths of different growing stages,
#'  and the second one with original onset data with phenological variables
#'  for the sites specified in \code{sitecode}.
#' @details Growing degree days for various crops are calculated using average daily minimum
#'  and maximum temperature values obtained from onset data.
#'  The temperature constraints specified in \code{base} and \code{max}
#'  are first applied before the calculations are done.
#'  These constraints ensure very low or high temperatures which prevent growth of a particular crop are not included.
#' Crops for which GDD calculations are available include: 'Barley', 'Bread wheat',
#'  'Chickpea', 'Durum wheat' and 'Lentil'. Each of these can be supplied as options for the argument \code{crop}.
#' Cumulative GDD values determine the length of different growing stages.
#'  Growing stages vary depending on the type of crop. Durum wheat, bread wheat
#'  and barley have five growth stages, i.e. beginning of heading,
#'  beginning and completion of flowering, and beginning and completion of grain filling.
#'  Chickpea and lentil have four growth stages, i.e. beginning of flowering,
#'  completion of 50% flowering, beginning of seed filling,
#'  and completion of 90% maturity (chickpea) or of full maturity (lentil).
#' The length of the full growth cycle of the crop for each site is also given in the output data frame.
#' @author Chafik Analy, Khadija Aouzal, Zakaria Kehel, Bancy Ngatia
#' @examples
#' \dontrun{
#'  # Calculate GDD for durum wheat
#'  data(durumDaily)
#'
#'  growth <- getGrowthPeriod(sitecode = durumDaily$site_code,
#'                            crop = 'Durum wheat', base = 0,
#'                            max = 35, gdd = TRUE)
#'
#'  # Get the dataframe with lengths of growth stages from the returned list
#'  growth.lengths <- growth[[1]]
#'
#'  # Get the dataframe with phenological variables from the returned list
#'  growth.pheno <- growth[[2]]
#'
#'  # Get the dataframe with GDD, cumulative GDD,
#'  # tmin and tmax from the returned list (when gdd = TRUE)
#'  growth.gdd <- growth[[3]]
#'  }
#'  
#' @name getGrowthPeriod
#' @importFrom stats setNames
#' @importFrom tidyr fill
#' @importFrom dplyr select mutate filter group_by slice
#' @importFrom dplyr arrange left_join row_number glimpse across lag ends_with
#' @importFrom plyr ddply join
#' @importFrom reshape2 melt
#' @export


getGrowthPeriod <- function(sitecode, crop, base, max, gdd = FALSE) {
  options(timeout = max(600, getOption("timeout")))
  
  # Validate input
  if (!is.character(crop) || !is.logical(gdd)) {
    stop("Crop must be a character string and gdd should be TRUE or FALSE.")
  }
  
  sitecode <- levels(as.factor(sitecode))
  # Map crop names to codes
  crop_codes <- c(
    "Durum wheat" = "ICDW", "Bread wheat" = "ICBW", 
    "Barley" = "ICB", "Chickpea" = "ILC", "Lentil" = "ILL"
  )
  onsetcrop <- crop_codes[crop]
  
  if (is.null(onsetcrop)) {
    stop("Invalid crop name. Available crops are: ", paste(names(crop_codes), collapse = ", "))
  }
  
  # Fetch onset data
  onsetdata <- getOnset(sites = unique(sitecode), var = c('tmin', 'tmax'), crop = onsetcrop)
  if (is.null(onsetdata) || length(onsetdata) < 2) {
    stop("No onset data available for the specified sites and crop. Please choose valid site names from the appropriate dataset.")
  }
  
  # Extract and process data
  climdata <- onsetdata[[1]]
  phenodata <- onsetdata[[2]]
  names(climdata)[grep("site", names(climdata))] <- "sitecode"
  names(phenodata)[grep("site", names(phenodata))] <- "sitecode"
  names(phenodata)[grep("onset", names(phenodata))] <- "onset"
  
  tminvars <- grep("tmin", names(climdata), value = TRUE)
  tmaxvars <- grep("tmax", names(climdata), value = TRUE)
  
  # Prepare cumulative GDD data
  prepareGDD <- function(data, base, tminvars, tmaxvars) {
    melted <- reshape2::melt(data, id.vars = "sitecode") |>
      arrange(sitecode) |>
      mutate(value = pmax(value, base, na.rm = TRUE))  # Handle missing values
    
    tmin_data <- melted[melted$variable %in% tminvars, ]
    tmax_data <- melted[melted$variable %in% tmaxvars, ]
    
    # Ensure lengths match
    if (nrow(tmin_data) != nrow(tmax_data)) {
      stop("Mismatch in the number of rows for tmin and tmax. Check the input data for consistency.")
    }
    
    # Calculate GDD
    gdd <- (tmin_data$value + tmax_data$value) / 2
    if (all(is.na(gdd))) 
      stop("GDD calculation failed: All values are NaN.")
    
    data.frame(
      sitecode = tmin_data$sitecode,
      tmin = tmin_data$value,
      tmax = tmax_data$value,
      gdd = gdd
     )|>
      group_by(sitecode)|>
      mutate(cumgdd = cumsum(gdd), DAP = row_number())
  }
  
  growth_period <- prepareGDD(climdata, base, tminvars, tmaxvars) |>
    left_join(phenodata, by = "sitecode")
  
  # Check if sitecode data is available after merging
  if (nrow(growth_period) == 0) {
    stop("No matching data found for the specified site names. Please verify the site names and try again.")
  }
  
  # Define thresholds for each crop
  thresholds <- list(
    "Durum wheat" = c(1567, 1739, 1768, 1825, 2170),
    "Bread wheat" = c(1567, 1739, 1768, 1825, 2170),
    "Barley" = c(1357, 1426, 1496, 1635, 1960),
    "Chickpea" = c(680, 870, 1075, 1740),
    "Lentil" = c(807, 980, 1187, 1808)
  )
  stage_names <- list(
    "Durum wheat" = c("HB", "FB", "FC", "GFB", "GFC"),
    "Bread wheat" = c("HB", "FB", "FC", "GFB", "GFC"),
    "Barley" = c("HB", "FB", "FC", "GFB", "GFC"),
    "Chickpea" = c("FB", "FC", "GFB", "GFC"),
    "Lentil" = c("FB", "FC50", "SF", "M")
  )
  
  # Calculate growth stages
  calculateStages <- function(data, cumgdd_values, stage_names) {
    results <- lapply(seq_along(cumgdd_values), function(i) {
      stage_df <- data |>
        group_by(sitecode) |>
        slice(which.min(abs(cumgdd - cumgdd_values[i])))
      
      stage_df[[stage_names[i]]] <- ifelse((stage_df$DAP + stage_df$onset) > 365,
                                           (stage_df$DAP + stage_df$onset) - 365, 
                                           stage_df$DAP + stage_df$onset)
      
      if (nrow(stage_df) == 0) {
        warning(paste("No data found for stage:", stage_names[i], "with threshold:", cumgdd_values[i]))
        return(NULL)
      }
      
      stage_df[, c("sitecode", "onset", stage_names[i])]
    })
    results <- results[!sapply(results, is.null)]  # Remove empty results
    if (length(results) == 0) stop("No data available for any growth stage.")
    Reduce(function(x, y) merge(x, y, by = c("sitecode", "onset"), all = TRUE), results)
  }
  
  # Validate crop thresholds
  if (any(thresholds[[crop]] > max(growth_period$cumgdd, na.rm = TRUE))) {
    stop("Threshold values exceed the maximum cumgdd in the data.")
  }
  
  stages <- calculateStages(growth_period, thresholds[[crop]], stage_names[[crop]])
  
  # Compute durations and cycles
  onset_and_stages <- data.frame(stage = names(stages)[-1])
  
  get_stage_length <- function(data, x, cur_column){
    previous_stages <- onset_and_stages |> 
      mutate(previous_stage = dplyr::lag(stage)) |>
      dplyr::filter(stage == cur_column)
    prev_stage <- previous_stages[["previous_stage"]]
    stage_length <- ifelse(x - data[[prev_stage]] < 0,
                           x - data[[prev_stage]] + 365,
                           x - data[[prev_stage]])
    return(stage_length)
  }
  
  stages_lengths <- stages |> mutate(across(-c(sitecode, onset), 
                                            ~ get_stage_length(.data, .x, cur_column()), 
                                            .names = "{.col}_length")) |>
    mutate(Cycle = rowSums(across(ends_with("_length"))))
  
  growth_period <- left_join(growth_period, stages_lengths |> select(sitecode, Cycle), by = "sitecode")
  dataCycle <- filter(growth_period, DAP <= Cycle)
  
  growing_degree_days <- dataCycle |> select(sitecode, tmin, tmax, gdd, cumgdd, DAP)
  
  list.df1 <- list(Growth_Period = stages_lengths, Onset_Data = phenodata)
  list.df2 <- list(Growth_Period = stages_lengths, Onset_Data = phenodata, Growing_Degree_Days = growing_degree_days)
  
  if (gdd) return(list.df2) else return(list.df1)
}

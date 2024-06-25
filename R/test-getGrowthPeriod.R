library(testthat)

source("/Volumes/Macintosh HD — Data/Desktop/FIGS/icardaFIGSr/R/getGrowthPeriod.R")

context("Testing getGrowthPeriod function")

# Sample data for sites
sites <- data.frame(
  site_id = 1:3,
  site_name = c("AFG-S296", "MAR85:7", "MAR85:55")
)

# Test: Function works with valid inputs
test_that("getGrowthPeriod works with valid inputs", {
  result <- getGrowthPeriod(sitecode = levels(as.factor(sites$site_name)),
                            crop = "Durum wheat", base = 0, max = 35, gdd = TRUE)
  expect_type(result, "list")
  expect_true(all(c("Growth_Period", "Onset_Data", "Growing_Degree_Days") %in% names(result)))
  expect_s3_class(result$Growth_Period, "data.frame")
  expect_s3_class(result$Onset_Data, "data.frame")
  expect_s3_class(result$Growing_Degree_Days, "data.frame")
})

# Test: Check error handling for invalid crop
test_that("getGrowthPeriod handles invalid crop", {
  expect_error(getGrowthPeriod(sitecode = levels(as.factor(sites$site_name)),
                               crop = "Invalid crop", base = 0, max = 35, gdd = TRUE),
               "No available data for your interest crop. Try the following crop codes :
         Durum wheat,Bread wheat, Barley, Chickpea, Lentil")
})

# Test: Check error handling for non-character crop
test_that("getGrowthPeriod handles non-character crop", {
  expect_error(getGrowthPeriod(sitecode = levels(as.factor(sites$site_name)),
                               crop = 123, base = 0, max = 35, gdd = TRUE),
               "Crop must be a single character string")
})


# Test: Check error handling for non-logical cv
test_that("getOnset handles non-logical gdd", {
  expect_error(getGrowthPeriod(sitecode = levels(as.factor(sites$site_name)),
                               crop = "Durum wheat", base = 0, max = 35, gdd = "yes"),
               "gdd should be TRUE or FALSE")
})

# Test: Check handling for valid crops with specific min and max values
valid_crops <- list(
  "Durum wheat" = list(base = 21, max = 35),
  "Bread wheat" = list(base = 21, max = 35),
  "Barley" = list(base = 21, max = 35),
  "Chickpea" = list(base = 10, max = 35),
  "Lentil" = list(base = 10, max = 35)
)

for (crop in names(valid_crops)) {
  crop_params <- valid_crops[[crop]]
  test_that(paste("getGrowthPeriod works with crop", crop), {
    result <- getGrowthPeriod(sitecode = levels(as.factor(sites$site_name)),
                              crop = crop, base = crop_params$base, max = crop_params$max, gdd = TRUE)
    expect_type(result, "list")
    expect_equal(length(result), 3)
    expect_true(all(c("Growth_Period", "Onset_Data", "Growing_Degree_Days") %in% names(result)))
    
  })
}

# Test: Check error handling for invalid site codes
test_that("getGrowthPeriod handles invalid site codes", {
  expect_error(getGrowthPeriod(sitecode = c("Invalid", "Site"), crop = "Durum wheat", base = 0, max = 35, gdd = TRUE),
               "No available data for interest sites. Make sure to use available sitecodes for your interest crop")
})

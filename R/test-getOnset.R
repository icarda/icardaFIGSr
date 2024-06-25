library(testthat)

source("/Volumes/Macintosh HD — Data/Desktop/FIGS/icardaFIGSr/R/getOnset.R")

context("Testing getOnset function")

# Sample data for sites
sites <- data.frame(
  SiteCode = c("MAR85:7","MAR85:55")
)

## Interest vars
vars = c("tavg", "prec", "rh")

# List of valid crops
valid_crops <- c("ICDW", "ICBW", "ICB")


# Test: Function works with valid inputs
test_that("getOnset works with valid inputs", {
  result <- getOnset(sites = unique(sites$SiteCode), crop = "ICDW",
                     var = vars ,cv = TRUE)
  
  expect_type(result, "list")
  expect_type(result[[1]],"list")
  expect_equal(length(result), length(vars))
  expect_equal(length(result[[1]][,1:2]), length(unique(sites$SiteCode)))
})

# Test: Check error handling for non-data frame sites
test_that("getOnset handles non-data frame sites", {
  expect_error(getOnset(sites = list("MAR85:7"), 
                        crop = "Durum wheat", var = "tavg", cv = TRUE), 
               "Sites must be a single or multiple character string")
})

# Test: Check error handling for non-character crop
test_that("getOnset handles non-character crop", {
  expect_error(getOnset(sites = unique(sites$SiteCode), crop = 123, var = "tavg", cv = TRUE), 
               "Crop must be a single character string. Check documentation for available crop codes.")
})

# Test: Check error handling for non-character var
test_that("getOnset handles non-character var", {
  expect_error(getOnset(sites = unique(sites$SiteCode), crop = "ICDW", var = 123, cv = TRUE), 
               "Variable must be a single or multiple character string : tavg, prec, rh")
})

# Test: Check error handling for non-logical cv
test_that("getOnset handles non-logical cv", {
  expect_error(getOnset(sites = unique(sites$SiteCode), crop = "ICDW", var = "tavg", cv = "yes"), 
               "cv should be TRUE or FALSE")
})

# Test: Check error handling for invalid crop names
test_that("getOnset handles invalid crop names", {
  expect_error(getOnset(sites = unique(sites$SiteCode), crop = "invalid_crop", var = "tavg", cv = TRUE), 
               "Crop must be a single character string. Check documentation for available crop codes.")
})


library(testthat)


source("/Volumes/Macintosh HD — Data/Desktop/FIGS/icardaFIGSr/R/extractWCdata.R")

context("Testing extractWCdata function")


# Sample data for sites
sites <- data.frame(
  lat = c(30, 31),
  lon = c(45, 46)
)

# Test: Function works with valid inputs
test_that("extractWCdata works with valid inputs", {
  result <- extractWCdata(sites = sites, long = "lon", lat = "lat", var = c("tavg", "prec"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_true(all(result$var %in% c("tavg", "prec")))
  expect_true(all(result$lat %in% sites$lat))
  expect_true(all(result$lon %in% sites$lon))
})

# Test: Check error handling for non-data frame sites
test_that("extractWCdata handles non-data frame sites", {
  expect_error(extractWCdata(sites = list(lat = 30, lon = 45), var = c("tavg", "prec")), 
               "Sites must be a data frame with 'lat' and 'lon' columns")
})

# Test: Check error handling for sites missing lat or lon columns
test_that("extractWCdata handles sites missing lat or lon columns", {
  expect_error(extractWCdata(sites = data.frame(latitude = 30, longitude = 45), var = c("tavg", "prec")), 
               "Sites must be a data frame with 'lat' and 'lon' columns")
})

# Test: Check error handling for non-numeric latitude
test_that("extractWCdata handles non-numeric latitude", {
  invalid_sites <- data.frame(lat = "30", lon = 45)
  expect_error(extractWCdata(sites = invalid_sites, var = c("tavg", "prec")), 
               "Latitude and longitude must be numeric")
})

# Test: Check error handling for non-numeric longitude
test_that("extractWCdata handles non-numeric longitude", {
  invalid_sites <- data.frame(lat = 30, lon = "45")
  expect_error(extractWCdata(sites = invalid_sites, var = c("tavg", "prec")), 
               "Latitude and longitude must be numeric")
})

# Test: Check error handling for non-character variables
test_that("extractWCdata handles non-character variables", {
  expect_error(extractWCdata(sites = sites, var = c(1, 2)), 
               "Variables must be a character vector")
})


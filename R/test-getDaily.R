library(testthat)

source("/Volumes/Macintosh HD — Data/Desktop/FIGS/icardaFIGSr/R/getDaily.R")

context("Testing getDaily function")

# Test: Check if function returns correct type based on `cv` parameter
test_that("Function returns correct output type", {
  
  output_cv_true <- getDaily(sites = c("MAR85:7", "MAR85:55"), var = c("tavg", "prec"), cv = TRUE)
  output_cv_false <- getDaily(sites = c("MAR85:7", "MAR85:55"), var = c("tavg", "prec"), cv = FALSE)
  
  expect_is(output_cv_true, "list")
  expect_is(output_cv_false, "data.frame")
})

# Test: Check if function returns correct column names when cv = FALSE
test_that("Function returns correct column names when cv = FALSE", {
  
  output <- getDaily(sites = c("MAR85:7", "MAR85:55"), var = c("tavg", "prec"), cv = FALSE)
  expected_colnames <- c("site_code", paste0("tavg", 1:365), paste0("prec", 1:365))
  
  expect_identical(colnames(output), expected_colnames)
})

# Test: Check if function returns correct list structure when cv = TRUE
test_that("Function returns correct list structure when cv = TRUE", {
  
  output <- getDaily(sites = c("MAR85:7", "MAR85:55"), var = c("tavg", "prec"), cv = TRUE)
  
  expect_is(output, "list")
  expect_length(output, 2)
  
  # Check the data frame structures within the list
  expect_is(output[[1]], "data.frame")
  expect_is(output[[2]], "data.frame")
  
})

# Test: Check error handling for invalid sites
test_that("Function handles invalid sites", {
  
  expect_error(getDaily(sites = c("invalid_site"), var = c("tavg", "prec"), cv = FALSE),
               "Invalid site_code. Refer to available site codes using getAcessions()")
})

# Test: Check error handling for invalid var
test_that("Function handles invalid var", {
  
  expect_error(getDaily(sites = c("MAR85:7", "MAR85:55"), var = c("invalid_var"), cv = FALSE),
               "var should be a single or multiple string among these : tavg, prec, rh")
})

# Test: Check error handling for invalid cv
test_that("Function handles invalid cv", {
  expect_error(getDaily(sites = c("MAR85:7", "MAR85:55"), var = c("tavg", "prec"), cv = "invalid_cv"), "cv should be TRUE or FALSE")
})

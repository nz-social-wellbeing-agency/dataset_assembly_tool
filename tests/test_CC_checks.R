###############################################################################
#' Description: Automated tests for checking confidentialisation functions
#'
#' Input: check_confidentiality.R
#'
#' Output: Test pass/fail report
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: testthat package, utility_functions.R
#'
#' Notes:
#'
#' Issues:
#'
#' History (reverse order):
#' 2022-08-22 SA v0
#' #############################################################################

#' Testing the following functions that check output is confidentialiseed
#' 
#' check_rounding_to_base_array(input_array, base = 3, na.rm = TRUE)
#' check_rounding_to_base_df(df, column, base = 3, na.rm = TRUE)
#' check_random_rounding(df, raw_col = NA, conf_col, base = 3)
#' check_small_count_suppression(df, suppressed_col, threshold, count_col = suppressed_col)
#' check_absence_of_zero_counts(df, conf_count_col, print_on_fail = TRUE)
#' expand_to_include_zero_counts(df)
#' check_confidentialised_results(df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
#' explore_output_report(df, output_dir = NA)
#' 
context("check confidentiality - checks")

#####################################################################
test_that("rounding vector passes & fails when expected", {
  # arrange
  vec3 = (1:14) * 3
  vec4 = (1:14) * 4
  vec17 = (27:82) * 17
  vec3na = c(vec3, NA)
  vec4na = c(vec4, NA)
  vec17na = c(vec17, NA)
  
  # act & assert
  expect_true(check_rounding_to_base_array(vec3, base = 3, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec4, base = 4, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec4, base = 2, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = FALSE))
  
  expect_true(check_rounding_to_base_array(vec3, base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4, base = 4, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4, base = 2, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = TRUE))
  
  expect_true(check_rounding_to_base_array(vec3na, base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4na, base = 4, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4na, base = 2, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = TRUE))
  
  # act & assert
  expect_false(check_rounding_to_base_array(vec3na, base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec4na, base = 4, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec4na, base = 2, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec17na, base = 17, na.rm = FALSE))
  
  expect_false(check_rounding_to_base_array(1:50, base = 2, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec3, base = 6, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec3na, base = 4, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec4na, base = 3, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec17, base = 7, na.rm = TRUE))
})

test_that("rounding vector errors when expected", {
  
  expect_error(check_rounding_to_base_array(1:50, base = "3", na.rm = TRUE), "numeric")
  expect_error(check_rounding_to_base_array(1:50, base = 3, na.rm = "TRUE"), "logical")
  expect_error(check_rounding_to_base_array(c(), base = 3, na.rm = TRUE), "no input")
  expect_error(check_rounding_to_base_array(c(), base = 3, na.rm = FALSE), "no input")
  expect_error(check_rounding_to_base_array(c(), base = 3, na.rm = TRUE), "no input")
  expect_error(check_rounding_to_base_array(c(1,2,3,NA,"NA"), base = 3, na.rm = TRUE), "numeric")
  
  expect_warning(check_rounding_to_base_array(c(NA, NA, NA), base = 3, na.rm = FALSE), "NA")
})

#####################################################################
test_that("rounding df passes & failed when expected", {
  # arrange
  input_df = data.frame(
    rr3 = 3 * 1:10,
    rr6 = 6 * 21:30,
    rrx = 41:50,
    rr3na = c(NA, 3 * 1:9),
    rr6na = c(6 * 61:69, NA)
  )
  
  # act & assert
  expect_true(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = TRUE))
  
  expect_false(check_rounding_to_base_df(input_df, "rr3na", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rr6na", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rr6na", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr3na", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6na", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6na", base = 6, na.rm = TRUE))
  
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 2, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 5, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 2, na.rm = TRUE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 3, na.rm = TRUE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 5, na.rm = TRUE))
})

test_that("rounding df errors when expected", {
  input_df = data.frame(rr3 = 3 * 1:5, rrx = 6:10, rr3na = c(3,6,3,9,NA), nas = c(NA, NA, NA, NA, NA))
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())

  expect_error(check_rounding_to_base_df("input_df", "rr3", base = 3, na.rm = FALSE), "dataset")
  expect_error(check_rounding_to_base_df(remote_df, "rr3", base = 3, na.rm = FALSE), "local")
  expect_error(check_rounding_to_base_df(input_df, 2, base = 3, na.rm = FALSE), "character")
  expect_error(check_rounding_to_base_df(input_df, "rr13", base = 3, na.rm = FALSE), "column name")
  
  # errors from passing column to vector
  expect_error(check_rounding_to_base_df(input_df, "rr3", base = "3", na.rm = TRUE), "numeric")
  expect_error(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = "TRUE"), "logical")
  
  expect_warning(check_rounding_to_base_df(input_df, "nas", base = 3, na.rm = TRUE), "NA")
})

#####################################################################
test_that("random rounding checks pass & fail when expected", {
  # arrange
  
  
  # check_random_rounding(df, raw_col = NA, conf_col, base = 3)
})



#####################################################################


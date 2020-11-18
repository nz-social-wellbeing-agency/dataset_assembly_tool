###############################################################################
#' Description: Automated tests for table consistency functions.
#'
#' Input: table_consistency_checks.R
#'
#' Output: Test pass/fail report
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: testthat package
#'
#' Notes:
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-05-04 SA v1
#' #############################################################################

#' Testing the following functions that operate independently
#'
#' num_missing_entries(df, col_name)
#' check_size_missing(df, col_name, direction, size)
#' assert_size_uniques(df, col_name, direction, size)
#' check_no_date_overlap(df, start_date, end_date, group_by_cols)
#'
context("table checks - missings and overlaps")

## correct number of missings ----
#
test_that("correct number of missings", {
  tmp <- data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))

  expect_equal(num_missing_entries(tmp, "a"), 3)
  expect_equal(num_missing_entries(tmp, "b"), 2)
  expect_equal(num_missing_entries(tmp, c("a", "b")), 5)
})

## number of missings is checked ----
#
test_that("number of missings is checked", {
  tmp <- data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))

  expect_true(check_size_missing(tmp, "a", "==", 3))
  expect_false(check_size_missing(tmp, "a", "==", 2))
  expect_true(check_size_missing(tmp, "b", ">=", 1))
  expect_true(check_size_missing(tmp, c("a", "b"), "==", 5))
})

## number of missings is asserted ----
#
test_that("number of missings is asserted", {
  tmp <- data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))

  expect_silent(assert_size_missing(tmp, "a", "==", 3))
  expect_error(assert_size_missing(tmp, "a", "==", 2))
  expect_silent(assert_size_missing(tmp, "b", ">=", 1))
  expect_silent(assert_size_missing(tmp, c("a", "b"), "==", 5))

  # required compoents of error message are present
  expect_error(assert_size_missing(tmp, "a", "==", 2), "==")
  expect_error(assert_size_missing(tmp, "a", "==", 2), "2")
})

## date overlap is checked ----
#
test_that("date overlap is checked", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    sd = as.Date(c("2010-01-01", "2011-01-01", "2013-01-01", "2014-01-01")),
    ed = as.Date(c("2010-06-06", "2015-01-01", "2013-06-06", "2014-06-06")),
    extra = c("a", "b", "d", "b")
  )

  expect_false(check_no_date_overlap(df, "sd", "ed", c()))
  expect_false(check_no_date_overlap(df, "sd", "ed", "extra"))

  expect_true(check_no_date_overlap(df, "sd", "ed", "id"))
  expect_true(check_no_date_overlap(df, "sd", "ed", c("id", "extra")))

  # duplicate dates
  df <- data.frame(
    id = c(1, 1, 2, 2),
    sd = as.Date(c("2010-01-01", "2011-01-01", "2010-01-01", "2014-01-01")),
    ed = as.Date(c("2010-06-06", "2011-01-01", "2010-06-06", "2014-06-06"))
  )

  expect_false(check_no_date_overlap(df, "sd", "ed", c()))
  expect_true(check_no_date_overlap(df, "sd", "ed", "id"))

  # start_date = end_date
  df <- data.frame(
    sd = as.Date(c("2010-01-01", "2010-06-06")),
    ed = as.Date(c("2010-06-06", "2011-01-01"))
  )

  expect_false(check_no_date_overlap(df, "sd", "ed", c()))
})

## invalid input is caught ----
#
test_that("invalid input is caught (with error type)", {
  tmp <- data.frame(a = c(1, 2, NA, NA, NA), b = c(NA, 2, 3, 4, NA))

  expect_error(num_missing_entries("tmp", "a"), "data.frame")
  expect_error(num_missing_entries(tmp, 1), "character")
  expect_error(num_missing_entries(tmp, "made_up_column_name"), "column name")

  expect_error(check_size_missing("tmp", "a", "==", 3), "data.frame")
  expect_error(check_size_missing(tmp, "made_up_column_name", "==", 3), "column name")
  expect_error(check_size_missing(tmp, "a", "wrong", 3), "direction")
  expect_error(check_size_missing(tmp, "a", "==", "2"), "numeric")

  expect_error(assert_size_missing("tmp", "a", "==", 3), "data.frame")
  expect_error(assert_size_missing(tmp, "made_up_column_name", "==", 3), "column name")
  expect_error(assert_size_missing(tmp, "a", "wrong", 3), "direction")
  expect_error(assert_size_missing(tmp, "a", "==", "2"), "numeric")

  df <- data.frame(
    sd = as.Date(c("2010-01-01", "2010-06-06")),
    ed = as.Date(c("2010-06-06", "2011-01-01"))
  )

  expect_error(check_no_date_overlap("df", "sd", "ed", c()), "data.frame")
  expect_error(check_no_date_overlap(df, 1, "ed", c()), "character")
  expect_error(check_no_date_overlap(df, c("sd", "sd"), "ed", c()), "single")
  expect_error(check_no_date_overlap(df, "1", "ed", c()), "column name")
  expect_error(check_no_date_overlap(df, "sd", 1, c()), "character")
  expect_error(check_no_date_overlap(df, "sd", c("ed", "ed"), c()), "single")
  expect_error(check_no_date_overlap(df, "sd", "1", c()), "column name")

  expect_error(check_no_date_overlap(df, "sd", "ed", 1), "character")
  expect_error(check_no_date_overlap(df, "sd", "ed", "1"), "column name")
})

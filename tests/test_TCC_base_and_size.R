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
#' 2020-04-15 SA v1
#' #############################################################################

#' Testing the following base and size related functions
#'
#' evaluation_comparison(val1, direction, val2)
#' num_row(df)
#' check_size(df, direction, size)
#' assert_size(df, direction, size)
#' check_size_comparison(df1, direction, df2)
#' assert_size_comparison(df1, direction, df2)
#'
context("table checks - base and size")

## Every direction works ----
#
test_that("every direction works", {
  expect_true(evaluation_comparison(1, "==", 1))
  expect_true(evaluation_comparison(0, "<=", 1))
  expect_true(evaluation_comparison(0, "<", 1))
  expect_true(evaluation_comparison(2, ">=", 1))
  expect_true(evaluation_comparison(2, ">", 1))
  expect_true(evaluation_comparison(2, "!=", 1))

  expect_false(evaluation_comparison(1, "==", 2))
  expect_false(evaluation_comparison(1, "<=", 0))
  expect_false(evaluation_comparison(1, "<", 0))
  expect_false(evaluation_comparison(1, ">=", 2))
  expect_false(evaluation_comparison(1, ">", 2))
  expect_false(evaluation_comparison(1, "!=", 1))
})

## Different ways to get size are equivalent ----
#
test_that("different ways to get size are equivalent", {
  data(mtcars)

  expect_equal(num_row(mtcars), nrow(mtcars))
  expect_equal(num_row(mtcars), length(mtcars[, 1]))
})

## size is checked ----
#
test_that("size is checked", {
  data(mtcars)
  nn <- nrow(mtcars)

  expect_true(check_size(mtcars, "==", nn))
  expect_false(check_size(mtcars, "!=", nn))
  expect_true(check_size(mtcars, ">=", 1))
})

## size is asserted ----
#
test_that("size is asserted", {
  data(mtcars)
  nn <- nrow(mtcars)

  expect_silent(assert_size(mtcars, "==", nn))
  expect_error(assert_size(mtcars, "!=", nn))
  expect_silent(assert_size(mtcars, ">=", 1))

  # required compoents of error message are present
  expect_error(assert_size(mtcars, "!=", nn), "!=")
  expect_error(assert_size(mtcars, "!=", nn), as.character(nn))
})

## size comparison is checked ----
#
test_that("size comparison is checked", {
  data(mtcars)
  mtcars2 <- dplyr::filter(mtcars, mpg <= 20)

  expect_true(check_size_comparison(mtcars, ">=", mtcars2))
  expect_true(check_size_comparison(mtcars, "!=", mtcars2))
  expect_true(check_size_comparison(mtcars, "==", mtcars))

  expect_false(check_size_comparison(mtcars, "==", mtcars2))
  expect_false(check_size_comparison(mtcars, "<", mtcars2))
})

## size comparison is asserts ----
#
test_that("size comparison is asserted", {
  data(mtcars)
  mtcars2 <- dplyr::filter(mtcars, mpg <= 20)
  n1 <- nrow(mtcars)
  n2 <- nrow(mtcars2)

  expect_silent(assert_size_comparison(mtcars, ">=", mtcars2))
  expect_silent(assert_size_comparison(mtcars, "!=", mtcars2))
  expect_silent(assert_size_comparison(mtcars, "==", mtcars))

  expect_error(assert_size_comparison(mtcars, "==", mtcars2))
  expect_error(assert_size_comparison(mtcars, "<", mtcars2))

  # required compoents of error message are present
  expect_error(assert_size_comparison(mtcars, "==", mtcars2), "==")
  expect_error(assert_size_comparison(mtcars, "==", mtcars2), as.character(n1))
  expect_error(assert_size_comparison(mtcars, "==", mtcars2), as.character(n2))
})

## invalid input caught ----
#
test_that("invalid input is caught (with error type)", {
  expect_error(evaluation_comparison(1, 1, 1), "direction")
  expect_error(evaluation_comparison(1, NA, 1), "direction")
  expect_error(evaluation_comparison(1, "==", NA), "val2")
  expect_error(evaluation_comparison(1, "==", NA), "numeric")
  expect_error(evaluation_comparison(NA, "==", 1), "val1")
  expect_error(evaluation_comparison(NA, "==", 1), "numeric")

  expect_error(num_row(1), "data.frame")
  expect_error(num_row("s"), "data.frame")
  expect_error(num_row(list("s")), "data.frame")

  data(mtcars)
  expect_error(check_size(mtcars, "==", NA), "numeric")
  expect_error(check_size(mtcars, "", 2), "direction")
  expect_error(check_size("mtcars", "==", 2), "data.frame")

  expect_error(assert_size(mtcars, "==", NA), "numeric")
  expect_error(assert_size(mtcars, "", 2), "direction")
  expect_error(assert_size("mtcars", "==", 2), "data.frame")

  expect_error(check_size_comparison("mtcars", "==", mtcars), "data.frame")
  expect_error(check_size_comparison(mtcars, "==", "mtcars"), "data.frame")
  expect_error(check_size_comparison(mtcars, "===", mtcars), "direction")

  expect_error(assert_size_comparison("mtcars", "==", mtcars), "data.frame")
  expect_error(assert_size_comparison(mtcars, "==", "mtcars"), "data.frame")
  expect_error(assert_size_comparison(mtcars, "===", mtcars), "direction")
})

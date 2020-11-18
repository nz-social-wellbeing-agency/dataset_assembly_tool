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

#' Testing the following functions that operate independently
#'
#' num_unique_entries(df, col_name)
#' check_size_uniques(df, col_name, direction, size)
#' assert_size_uniques(df, col_name, direction, size)
#' check_all_unique(df, col_name)
#' assert_all_unique(df, col_name)
#' check_join_covered(df1, df2, join_col)
#' assert_join_covered(df1, df2, join_col)
#'
context("table checks - uniques and joins")

## different ways to check number of uniques are equivalent ----
#
test_that("different ways to check number of uniques are equivalent", {
  data(mtcars)

  expect_equal(num_unique_entries(mtcars, "mpg"), dplyr::n_distinct(mtcars$mpg))
  expect_equal(num_unique_entries(mtcars, "mpg"), length(unique(mtcars$mpg)))
})

## number of uniques is checked ----
#
test_that("number of uniques is checked", {
  data(mtcars)

  expect_true(check_size_uniques(mtcars, "gear", "==", 3))
  expect_false(check_size_uniques(mtcars, "gear", "==", 2))
  expect_true(check_size_uniques(mtcars, "mpg", ">", 1))
})

## number of uniques is asserted ----
#
test_that("number of uniques is asserted", {
  data(mtcars)

  expect_silent(assert_size_uniques(mtcars, "gear", "==", 3))
  expect_silent(assert_size_uniques(mtcars, "mpg", ">", 1))
  expect_error(assert_size_uniques(mtcars, "gear", "==", 2))

  # required compoents of error message are present
  expect_error(assert_size_uniques(mtcars, "gear", "==", 2), "==")
  expect_error(assert_size_uniques(mtcars, "gear", "==", 2), "2")
})

## all uniques is checked ----
#
test_that("all uniques is checked", {
  my_data <- data.frame(uu = c(1, 2, 3, 4, 5, 6), vv = c("a", "b", "a", "b", "c", "b"))

  expect_true(check_all_unique(my_data, "uu"))
  expect_false(check_all_unique(my_data, "vv"))
})

## all uniques is asserted ----
#
test_that("all uniques is asserted", {
  my_data <- data.frame(uu = c(1, 2, 3, 4, 5, 6), vv = c("a", "b", "a", "b", "c", "b"))

  expect_silent(assert_all_unique(my_data, "uu"))
  expect_error(assert_all_unique(my_data, "vv"), "vv")
  expect_error(assert_all_unique(my_data, "vv"), "not all entries")
})

## join coverage is checked ----
#
test_that("join coverage is checked", {
  df1 <- data.frame(a = c(1, 2, 3, 4, 5), b = c(1, 2, 3, 1, 2))
  df2 <- data.frame(a = c(1, 2, 3, 4, 4), z = c(3, 3, 2, 3, 2))

  expect_true(check_join_covered(df2, df1, "a"))
  expect_false(check_join_covered(df1, df2, "a"))
  expect_true(check_join_covered(df2, df1, c(z = "b")))
  expect_false(check_join_covered(df1, df2, c(b = "z")))
})

## join coverage is asserted ----
#
test_that("join coverage is asserted", {
  df1 <- data.frame(a = c(1, 2, 3, 4, 5), b = c(1, 2, 3, 1, 2))
  df2 <- data.frame(a = c(1, 2, 3, 4, 4), z = c(3, 3, 2, 3, 2))

  expect_silent(assert_join_covered(df2, df1, "a"))
  expect_silent(assert_join_covered(df2, df1, c(z = "b")))

  expect_error(assert_join_covered(df1, df2, "a"), "not covered")
  expect_error(assert_join_covered(df1, df2, c(b = "z")), "b")
  expect_error(assert_join_covered(df1, df2, c(b = "z")), "z")
})

## invalid input is caught ----
#
test_that("invalid input is caught (with error type)", {
  data(mtcars)

  expect_error(num_unique_entries("mtcars", "gear"), "data.frame")
  expect_error(num_unique_entries(mtcars, 1), "character")
  expect_error(num_unique_entries(mtcars, "made_up_column_name"), "column name")

  expect_error(check_size_uniques("mtcars", "gear", "==", 3), "data.frame")
  expect_error(check_size_uniques(mtcars, "made_up_column_name", "==", 3), "column name")
  expect_error(check_size_uniques(mtcars, "gear", "wrong", 3), "direction")
  expect_error(check_size_uniques(mtcars, "gear", "==", "2"), "numeric")

  expect_error(assert_size_uniques("mtcars", "gear", "==", 3), "data.frame")
  expect_error(assert_size_uniques(mtcars, "made_up_column_name", "==", 3), "column name")
  expect_error(assert_size_uniques(mtcars, "gear", "wrong", 3), "direction")
  expect_error(assert_size_uniques(mtcars, "gear", "==", "2"), "numeric")

  expect_error(check_all_unique("mtcars", "gear"), "data.frame")
  expect_error(check_all_unique(mtcars, "made_up_column_name"), "column name")

  expect_error(assert_all_unique("mtcars", "gear"), "data.frame")
  expect_error(assert_all_unique(mtcars, "made_up_column_name"), "column name")

  expect_error(check_join_covered("mtcars", mtcars, "mpg"))
  expect_error(check_join_covered(mtcars, "mtcars", "mpg"))
  expect_error(check_join_covered(mtcars, mtcars, "made_up_column_name"))

  expect_error(assert_join_covered("mtcars", mtcars, "mpg"))
  expect_error(assert_join_covered(mtcars, "mtcars", "mpg"))
  expect_error(assert_join_covered(mtcars, mtcars, "made_up_column_name"))
})

## multiple columns can be input for unique and join ----
#
test_that("multiple columns can be input for unique and join", {
  data(mtcars)
  cols <- c("gear", "am")

  expect_equal(num_unique_entries(mtcars, cols), dplyr::n_distinct(mtcars[, cols]))

  expect_true(check_size_uniques(mtcars, cols, "==", 4))
  expect_error(assert_size_uniques(mtcars, cols, "!=", 4), "gear, am")

  expect_false(check_all_unique(mtcars, cols))
  expect_error(assert_all_unique(mtcars, cols), "gear, am")

  df1 <- data.frame(a = c(1, 2, 3, 4, 5, 4), b = c(1, 2, 3, 1, 2, 2), a2 = c(3, 3, 2, 3, 2, 2))
  df2 <- data.frame(y = c(1, 2, 3, 4, 4), z = c(3, 3, 2, 3, 2))

  expect_true(check_join_covered(df2, df1, c(y = "a", z = "a2")))
  expect_false(check_join_covered(df1, df2, c(a = "y", b = "z")))

  expect_error(assert_join_covered(df1, df2, c(a = "y", b = "z")), "a, b")
  expect_error(assert_join_covered(df1, df2, c(a = "y", b = "z")), "y, z")
})

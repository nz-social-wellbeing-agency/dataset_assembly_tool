###############################################################################
#' Description: Automated tests for overview dataset functions.
#'
#' Input: overview_dataset.R
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
#' 2021-09-08 SA v0
#' #############################################################################

#' Testing the following functions for producing automated reports
#'
#' filter_to_limited_number_of_rows(df, row_limit = 10000, id_column = NA)
#' 
context("overview dataset - filter to limited number of rows")

test_that("filtering of small tables returns in full", {
  # arrange
  start_df = data.frame(id = 1:100,
                        col1 = rep(1:4, 25),
                        col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25)))

  # act
  out_df1 = filter_to_limited_number_of_rows(start_df, row_limit = 100)
  out_df2 = filter_to_limited_number_of_rows(start_df, row_limit = 101)
  out_df3 = filter_to_limited_number_of_rows(start_df, row_limit = 1000)
  
  # assert
  expect_true(all.equal(start_df, out_df1, ignore_row_order = TRUE, ignore_col_order = FALSE))
  expect_true(all.equal(start_df, out_df2, ignore_row_order = TRUE, ignore_col_order = FALSE))
  expect_true(all.equal(start_df, out_df3, ignore_row_order = TRUE, ignore_col_order = FALSE))
})

test_that("no id returns top rows", {
  # arrange
  start_df = data.frame(id = 1:100,
                        col1 = rep(1:4, 25),
                        col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25)))
  
  # act
  out_df1 = filter_to_limited_number_of_rows(start_df, row_limit = 1)
  out_df2 = filter_to_limited_number_of_rows(start_df, row_limit = 10)
  out_df3 = filter_to_limited_number_of_rows(start_df, row_limit = 50)
  
  # assert
  expect_true(all.equal(head(start_df, 1), out_df1, ignore_row_order = TRUE, ignore_col_order = FALSE))
  expect_true(all.equal(head(start_df, 10), out_df2, ignore_row_order = TRUE, ignore_col_order = FALSE))
  expect_true(all.equal(head(start_df, 50), out_df3, ignore_row_order = TRUE, ignore_col_order = FALSE))
})

test_that("filtering occurs by id", {
  # arrange
  ROW_LIMIT = 10
  start_df = data.frame(id = 1:100,
                        col1 = rep(1:4, 25),
                        col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25)))
  
  # act
  out_df = filter_to_limited_number_of_rows(start_df, row_limit = ROW_LIMIT, id_column = "id")
  
  out_ids = sort(out_df$id)
  # assert
  expect_false(isTRUE(all.equal(head(start_df, nrow(out_df)), out_df, ignore_row_order = TRUE, ignore_col_order = TRUE)))
  expect_true(min(diff(out_ids)) > ROW_LIMIT / 2)
})

test_that("filtering by id can return different numbers of rows", {
  # arrange
  start_df = data.frame(id = 1:100,
                        col1 = rep(1:4, 25),
                        col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25)))
  # act
  out_df1 = filter_to_limited_number_of_rows(start_df, row_limit = 29, id_column = "id")
  out_df2 = filter_to_limited_number_of_rows(start_df, row_limit = 71, id_column = "id")
  out_df3 = filter_to_limited_number_of_rows(start_df, row_limit = 17, id_column = "id")
  # assert
  expect_true(nrow(out_df1) <= 29)
  expect_true(nrow(out_df2) <= 71)
  expect_true(nrow(out_df3) <= 17)
  expect_true(abs(nrow(out_df1) - nrow(out_df2)) > 5)
  expect_true(abs(nrow(out_df3) - nrow(out_df2)) > 5)
  expect_true(abs(nrow(out_df1) - nrow(out_df3)) > 5)
})

test_that("input checks stop execution", {
  start_df = data.frame(id = 1:100,
                        col1 = rep(1:4, 25),
                        col2 = c(rep("a",25), rep("b",25), rep("c",25), rep("d", 25)))
  
  expect_error(filter_to_limited_number_of_rows(rep(1:100)), "data\\.frame")
  expect_error(filter_to_limited_number_of_rows("start_df"), "data\\.frame")
  expect_error(filter_to_limited_number_of_rows(start_df, "10"), "numeric")
  expect_error(filter_to_limited_number_of_rows(start_df, NA), "numeric")
  expect_error(filter_to_limited_number_of_rows(start_df, 10, 2), "string")
  expect_error(filter_to_limited_number_of_rows(start_df, 10, "not_a_column"), "column name")
})

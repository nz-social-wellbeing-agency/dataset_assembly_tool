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
#' explore_report(df, id_column = NA, target = NA, output_file = NA, output_dir = NA)
#' 
context("overview dataset - explore report")

test_that("reports are produced", {
  # arrange
  data(iris)
  start_df = data.frame(id = 1:20000,
                        col1 = rep(1:4, 5000),
                        col2 = sample(c("a","b","c","d","e","f","g"), 20000, replace = TRUE))

  # act - limit to three for speed
  iris_simple = explore_report(iris, id_column = NA, target = NA, output_file = "iris_simple")
  df_col1 = explore_report(start_df, id_column = NA, target = "col1", output_file = "df_col1")
  df_id_col2 = explore_report(start_df, id_column = "id", target = "col2", output_file = "df_id_col2")
  
  # assert
  expect_true(iris_simple %in% list.files())
  expect_true(df_col1 %in% list.files())
  expect_true(df_id_col2 %in% list.files())
  
  # tidy
  unlink(c(iris_simple, df_col1, df_id_col2))
})

test_that("input checks stop execution", {
  start_df = data.frame(id = 1:200,
                        col1 = rep(1:4, 50),
                        col2 = sample(c("a","b","c","d","e","f","g"), 200, replace = TRUE))
  
  expect_error(explore_report("start_df"), "data\\.frame")
  expect_error(explore_report(start_df, id_column = 4), "string")
  expect_error(explore_report(start_df, target = 4), "string")
  expect_error(explore_report(start_df, target = "not_a_column"), "column name")
  expect_error(explore_report(start_df, output_file = 123), "string")
  expect_error(explore_report(start_df, output_dir = 123), "string")
})


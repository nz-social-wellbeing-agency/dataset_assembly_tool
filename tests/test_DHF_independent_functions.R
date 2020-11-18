###############################################################################
#' Description: Automated tests for dbplyr helper functions.
#'
#' Input: dbplyr_helper_functions.R
#'
#' Output: Test pass/fail report
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: testthat package, utility_functions.R
#'
#' Notes:
#' - Some functions (and tests) assume SQL Server environment
#'   due to differences in syntax between different implementations of SQL.
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-01-06 SA v1
#' 2019-11-20 SA v0
#' #############################################################################

#' Testing the following functions that operate independently
#'
#' save_to_sql(query, desc)
#' table_contains_required_columns(tbl_to_check, required_columns, only = FALSE)
#' db_schema(db, schema)
#' warn_if_missing_delimiters(db, schema, tbl_name)
#'
context("dbplyr helpers - independent functions")

test_that("sql files written", {
  # arrange
  tmp_directory <- "./SQL tmp scripts"
  folder_exists_to_start <- dir.exists(tmp_directory)
  file_name <- paste0("test", floor(runif(1, 1000000, 9999999)))

  # act
  save_to_sql("placeholder query", file_name)
  folder_exists <- dir.exists(tmp_directory)
  file_exists <- any(grepl(file_name, list.files(tmp_directory)))
  file_to_remove <- dir(tmp_directory, pattern = file_name)
  file.remove(paste0(tmp_directory, "/", file_to_remove))
  if (!folder_exists_to_start) {
    unlink(tmp_directory, recursive = TRUE)
  }
  folder_exists_at_end <- dir.exists(tmp_directory)

  # assert
  expect_true(file_exists)
  expect_true(folder_exists)
  expect_true(folder_exists_to_start == folder_exists_at_end)
})

test_that("column names are checked", {
  data(iris)
  cols <- colnames(iris)

  expect_true(table_contains_required_columns(iris, cols))
  expect_true(table_contains_required_columns(iris, cols[1]))
  expect_true(table_contains_required_columns(iris, cols, only = TRUE))
  expect_false(table_contains_required_columns(iris, cols[1], only = TRUE))
  expect_false(table_contains_required_columns(iris, c(cols, "asdasdasd"), only = TRUE))
})

test_that("db and schema are delimited", {
  expect_error(db_schema("foo", "bar"))
  expect_error(db_schema("foo", "[bar]"))
  expect_error(db_schema("[foo]", "bar"))
  expect_error(db_schema("foo", "\"bar\""))
  expect_error(db_schema("\"foo\"", "bar"))
})

test_that("db and schema are combined", {
  expect_equal(db_schema("[foo]", "[bar]"), "[foo].[bar]")
  expect_error(db_schema('"foo"', '"bar"'))
  expect_error(db_schema("[foo]", '"bar"'))
})

test_that("non-delimited sql objects produce warnings", {
  expect_silent(warn_if_missing_delimiters("[db]", "[schema]", "[tbl_name]"))
  expect_warning(warn_if_missing_delimiters("db", "[schema]", "[tbl_name]"), "db")
  expect_warning(warn_if_missing_delimiters("[db]", "schema", "[tbl_name]"), "schema")
  expect_warning(warn_if_missing_delimiters("[db]", "[schema]", "tbl_name"), "tbl_name")
})

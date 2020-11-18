###############################################################################
#' Description: Automated tests for general assembly tool help functions.
#'
#' Input: general_assembly_tool_functions.R
#'
#' Output: Test pass/fail report
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: testthat package
#'
#' Notes:
#' - Some functions (and tests) assume SQL Server environment
#'   due to differences in syntax between different implementations of SQL.
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-01-06 SA v1
#' 2019-12-13 SA v0
#' #############################################################################

#' Testing the following functions that handle text for SQL
#'
#' no_internal_delimiters(string, table_label)
#' prep_for_sql(string, alias)
#'
context("assembler - text handling")

test_that("warn only for internal delimiters", {
  string <- "[str].[ing]"
  expect_warning(no_internal_delimiters(string, "tbl"))
  string <- "[str]ing]"
  expect_warning(no_internal_delimiters(string, "tbl"))

  for (string in c("[string]", "\"string\"", "string")) {
    expect_silent(no_internal_delimiters(string, "tbl"))
  }
})

test_that("double quotes removed", {
  expect_equal(prep_for_sql('"foobar"', "k"), "'foobar'")
  expect_equal(prep_for_sql("\"foobar\"", "k"), "'foobar'")
})

test_that("alias appended", {
  expect_equal(prep_for_sql("[foobar]", "k"), "k.[foobar]")
})

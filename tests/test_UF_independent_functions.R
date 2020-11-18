###############################################################################
#' Description: Automated tests for utility functions.
#'
#' Input: utility_functions.R
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
#' 2019-11-20 SA v0
#' #############################################################################

#' Testing the following functions that operate independently
#'
#' assert(condition, msg)
#' run_time_inform_user(msg)
#' '%not_in%'(x,y)
#' no_special_characters(in_string)
#' is_delimited(string, delimiter)
#' has_internal_delimiters(string, delimiter)
#' remove_delimiters(string, delimiter)
#' add_delimiters(string, delimiter)
#'
context("utility - independent functions")

test_that("messages outputed", {
  expect_output(run_time_inform_user("message text"), as.character(Sys.Date()))
  expect_output(run_time_inform_user("message text"), "message text")
})

test_that("messages are conditional", {
  msg <- "message text"
  PRINT_LEVEL <- "all"
  expect_output(run_time_inform_user(msg, print_level = PRINT_LEVEL), msg)
  expect_output(run_time_inform_user(msg, context = "heading", print_level = PRINT_LEVEL), toupper(msg))
  expect_output(run_time_inform_user(msg, context = "details", print_level = PRINT_LEVEL), msg)
  expect_output(run_time_inform_user(msg, context = "all", print_level = PRINT_LEVEL), msg)
  PRINT_LEVEL <- "details"
  expect_output(run_time_inform_user(msg, print_level = PRINT_LEVEL), msg)
  expect_output(run_time_inform_user(msg, context = "heading", print_level = PRINT_LEVEL), toupper(msg))
  expect_output(run_time_inform_user(msg, context = "details", print_level = PRINT_LEVEL), msg)
  expect_silent(run_time_inform_user(msg, context = "all", print_level = PRINT_LEVEL))
  PRINT_LEVEL <- "heading"
  expect_output(run_time_inform_user(msg, print_level = PRINT_LEVEL), msg)
  expect_output(run_time_inform_user(msg, context = "heading", print_level = PRINT_LEVEL), toupper(msg))
  expect_silent(run_time_inform_user(msg, context = "details", print_level = PRINT_LEVEL))
  expect_silent(run_time_inform_user(msg, context = "all", print_level = PRINT_LEVEL))
  PRINT_LEVEL <- "none"
  expect_silent(run_time_inform_user(msg, print_level = PRINT_LEVEL))
  expect_silent(run_time_inform_user(msg, context = "heading", print_level = PRINT_LEVEL))
  expect_silent(run_time_inform_user(msg, context = "details", print_level = PRINT_LEVEL))
  expect_silent(run_time_inform_user(msg, context = "all", print_level = PRINT_LEVEL))
})

test_that("assert performs", {
  msg <- "message text"
  expect_error(assert(FALSE, msg), msg)
  expect_silent(assert(TRUE, msg))
})

test_that("special characters rejected", {
  expect_error(no_special_characters("foo (bar)"), "special characters")
  expect_error(no_special_characters("foo bar"), "white space")
  expect_error(no_special_characters("foo:bar"), "special characters")
})

test_that("no special characters passed", {
  a1 <- no_special_characters("foo_bar")
  expect_identical(a1, NULL)
})

test_that("multivariate LHS is correct", {
  not_in1 <- c(1, 2) %not_in% c(1, 2, 3)
  not_in2 <- c(8, 2) %not_in% c(1, 2, 3)
  in1 <- c(1, 2) %in% c(1, 2, 3)
  in2 <- c(8, 2) %in% c(1, 2, 3)

  expect_equal(not_in1, !in1)
  expect_equal(not_in2, !in2)
})

test_that("different calls work", {
  b1 <- c("a", "b", "c")
  b2 <- c("c", "d", "e")

  expect_equal(b1 %not_in% b2, `%not_in%`(b1, b2))
})

test_that("delimiters are checked", {
  expect_true(is_delimited("[string]", "[]"))
  expect_true(is_delimited('"string"', "\""))
  expect_true(is_delimited('"[string]"', "\""))
  expect_true(is_delimited('"string"', "\""))
  expect_true(is_delimited("\"string\"", "\""))
})

test_that("delimiters are not muddled", {
  expect_false(is_delimited("[string]", "\""))
  expect_false(is_delimited('"string"', "[]"))
  expect_false(is_delimited('"[string]"', "[]"))
  expect_false(is_delimited('"string"', "[]"))
  expect_false(is_delimited("\"string\"", "[]"))
})

test_that("non-sql delimiters work", {
  expect_true(is_delimited("astringa", "a"))
  expect_true(is_delimited("astringb", "ab"))
  expect_true(is_delimited("astringb", "a b"))
  expect_false(is_delimited(" string ", "a b"))
  expect_false(is_delimited("astring ", "a b"))
  expect_false(is_delimited(" stringb", "a b"))
})

test_that("regular delimiter cases pass", {
  expect_false(has_internal_delimiters("[string]", "[]"))
  expect_false(has_internal_delimiters("\"string\"", "\""))
  expect_false(has_internal_delimiters('"string"', "\""))
})

test_that("internal delimiters detected", {
  expect_true(has_internal_delimiters("[str].[ing]", "[]"))
  expect_true(has_internal_delimiters("[str]ing]", "[]"))
  expect_true(has_internal_delimiters("\"str\".\"ing\"", "\""))
  expect_true(has_internal_delimiters("\"str\"ing\"", "\""))
  expect_true(has_internal_delimiters("\"str'ing\"", "\""))
})

test_that("delimiters removed", {
  expect_equal(remove_delimiters("[text]", "[]"), "text")
  expect_equal(remove_delimiters("[odd][text]", "[]"), "odd][text")
  expect_equal(remove_delimiters("[text]", "\""), "[text]")
  expect_equal(remove_delimiters("\"text\"", "[]"), "\"text\"")
  expect_equal(remove_delimiters("\"text\"", "\""), "text")
  expect_equal(remove_delimiters("[text]", "[filler]"), "text")
  expect_equal(remove_delimiters("text", "[]"), "text")
  expect_equal(remove_delimiters("text", "t"), "ex")
  expect_equal(remove_delimiters("[text", "[]"), "text")
  expect_equal(remove_delimiters("text]", "[]"), "text")
})

test_that("delimiters added", {
  expect_equal(add_delimiters("text", "[]"), "[text]")
  expect_equal(add_delimiters("text", "[filler]"), "[text]")
  expect_equal(add_delimiters("[text", "[]"), "[text]")
  expect_equal(add_delimiters("text]", "[]"), "[text]")
  expect_equal(add_delimiters("[text]", "[]"), "[text]")

  expect_equal(add_delimiters("[odd][text]", "[]"), "[odd][text]")
  expect_equal(add_delimiters("[text]", "\""), "\"[text]\"")
  expect_equal(add_delimiters("\"text\"", "[]"), "[\"text\"]")
})

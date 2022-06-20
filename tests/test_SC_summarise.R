###############################################################################
#' Description: Automated tests for summarise and confidentialise functions
#'
#' Input: summary_confidential.R
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
#' 2021-09-13 SA v0
#' #############################################################################

#' Testing the following functions that summarise output
#' 
#' summarise_and_label(df, group_by_cols, summarise_col,
#'                     make_distinct, make_count, make_sum,
#'                     clean = "none", remove.na.from.groups = TRUE)
#' summarise_and_label_over_lists(df, group_by_list, summarise_list,
#'                                make_distinct, make_count, make_sum,
#'                                clean = "none", remove.na.from.groups = TRUE)
#'                                
context("summary confidential - summarise")

###############################################################################
test_that("individual summarise runs", {
  # arrange
  input_df = tibble::tibble(your_label = c(rep("z",50), rep("y",50)),
                            my_label = rep(c("a","b"), 50),
                            rownum = 1:100,
                            values = rep(c(0,0,NA,1,2), 20))
  expected_output_rownum = tibble::tibble(col01 = rep("your_label", 4),
                                          col02 = rep("my_label", 4),
                                          val01 = c("z","y","z","y"),
                                          val02 = c("a","a","b","b"),
                                          summarised_var = rep("rownum", 4),
                                          distinct = rep(25, 4),
                                          count = rep(25, 4),
                                          sum = c(625, 1875, 650, 1900))
  
  # act
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = c("your_label", "my_label"),
                                           summarise_col = "rownum",
                                           make_distinct = TRUE,
                                           make_count = TRUE,
                                           make_sum = TRUE,
                                           clean = "none",
                                           remove.na.from.groups = TRUE)
  # assert
  expect_true(all_equal(this_actual_output, expected_output_rownum, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})


test_that("individual summarise type control trigger", {
  # arrange
  input_df = tibble::tibble(your_label = c(rep("z",50), rep("y",50)),
                            my_label = rep(c("a","b"), 50),
                            rownum = 1:100,
                            values = rep(c(0,0,NA,1,2), 20))
  expected_output_rownum = tibble::tibble(col01 = rep("your_label", 4),
                                          col02 = rep("my_label", 4),
                                          val01 = c("z","y","z","y"),
                                          val02 = c("a","a","b","b"),
                                          summarised_var = rep("rownum", 4),
                                          distinct = rep(25, 4),
                                          count = rep(25, 4),
                                          sum = c(625, 1875, 650, 1900))
  
  # act & assert
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = c("your_label", "my_label"),
                                           summarise_col = "rownum",
                                           make_distinct = TRUE,
                                           make_count = TRUE,
                                           make_sum = TRUE,
                                           clean = "none",
                                           remove.na.from.groups = TRUE)
  expect_true(all_equal(this_actual_output, expected_output_rownum, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = c("your_label", "my_label"),
                                           summarise_col = "rownum",
                                           make_distinct = FALSE,
                                           make_count = TRUE,
                                           make_sum = TRUE,
                                           clean = "none",
                                           remove.na.from.groups = TRUE)
  this_expect = dplyr::select(expected_output_rownum, -distinct)
  expect_true(all_equal(this_actual_output, this_expect, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = c("your_label", "my_label"),
                                           summarise_col = "rownum",
                                           make_distinct = TRUE,
                                           make_count = FALSE,
                                           make_sum = TRUE,
                                           clean = "none",
                                           remove.na.from.groups = TRUE)
  this_expect = dplyr::select(expected_output_rownum, -count)
  expect_true(all_equal(this_actual_output, this_expect, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = c("your_label", "my_label"),
                                           summarise_col = "rownum",
                                           make_distinct = TRUE,
                                           make_count = TRUE,
                                           make_sum = FALSE,
                                           clean = "none",
                                           remove.na.from.groups = TRUE)
  this_expect = dplyr::select(expected_output_rownum, -sum)
  expect_true(all_equal(this_actual_output, this_expect, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})


test_that("individual summarise NA controls trigger", {
  # arrange
  input_df = tibble::tibble(your_label = c(rep("z",50), rep("y",50)),
                            my_label = rep(c("a","b"), 50),
                            rownum = 1:100,
                            values = rep(c(0,0,NA,1,2), 20))
  expected_output_none = tibble::tibble(col01 = rep("your_label", 4),
                                        col02 = rep("my_label", 4),
                                        val01 = c("z","y","z","y"),
                                        val02 = c("a","a","b","b"),
                                        summarised_var = rep("values", 4),
                                        distinct = rep(3, 4),
                                        count = rep(20, 4),
                                        sum = rep(15, 4))
  expected_output_NA = tibble::tibble(col01 = rep("your_label", 4),
                                      col02 = rep("my_label", 4),
                                      val01 = c("z","y","z","y"),
                                      val02 = c("a","a","b","b"),
                                      summarised_var = rep("values", 4),
                                      distinct = rep(2, 4),
                                      count = rep(10, 4),
                                      sum = rep(15, 4))
  expected_output_zero = tibble::tibble(col01 = rep("your_label", 4),
                                        col02 = rep("my_label", 4),
                                        val01 = c("z","y","z","y"),
                                        val02 = c("a","a","b","b"),
                                        summarised_var = rep("values", 4),
                                        distinct = rep(3, 4),
                                        count = rep(25, 4),
                                        sum = rep(15, 4))
  
  # act & assert
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = c("your_label", "my_label"),
                                           summarise_col = "values",
                                           make_distinct = TRUE,
                                           make_count = TRUE,
                                           make_sum = TRUE,
                                           clean = "none",
                                           remove.na.from.groups = TRUE)
  expect_true(all_equal(this_actual_output, expected_output_none, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = c("your_label", "my_label"),
                                           summarise_col = "values",
                                           make_distinct = TRUE,
                                           make_count = TRUE,
                                           make_sum = TRUE,
                                           clean = "zero.as.na",
                                           remove.na.from.groups = TRUE)
  expect_true(all_equal(this_actual_output, expected_output_NA, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = c("your_label", "my_label"),
                                           summarise_col = "values",
                                           make_distinct = TRUE,
                                           make_count = TRUE,
                                           make_sum = TRUE,
                                           clean = "na.as.zero",
                                           remove.na.from.groups = TRUE)
  expect_true(all_equal(this_actual_output, expected_output_zero, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})


test_that("individual summarise remove NA from groups", {
  # arrange
  input_df = tibble::tibble(your_label = c(rep("z",50), rep("y",50)),
                            my_label = rep(c("a","b"), 50),
                            rownum = 1:100,
                            values = rep(c(0,0,NA,1,2), 20))
  expected_output_NA = tibble::tibble(col01 = rep("values", 4),
                                      val01 = c("0","1","2",NA),
                                      summarised_var = rep("rownum", 4),
                                      sum = c(1960,1030,1050,1010))
  expected_output_none = dplyr::filter(expected_output_NA, !is.na(val01))
  
  
  # act & assert
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = "values",
                                           summarise_col = "rownum",
                                           make_distinct = FALSE,
                                           make_count = FALSE,
                                           make_sum = TRUE,
                                           clean = "none",
                                           remove.na.from.groups = TRUE)
  expect_true(all_equal(this_actual_output, expected_output_none, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  this_actual_output = summarise_and_label(df = input_df,
                                           group_by_cols = "values",
                                           summarise_col = "rownum",
                                           make_distinct = FALSE,
                                           make_count = FALSE,
                                           make_sum = TRUE,
                                           clean = "none",
                                           remove.na.from.groups = FALSE)
  expect_true(all_equal(this_actual_output, expected_output_NA, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})

test_that("input checks stop execution", {
  input_df = tibble::tibble(your_label = c(rep("z",50), rep("y",50)),
                            my_label = rep(c("a","b"), 50),
                            rownum = 1:100,
                            values = rep(c(0,0,NA,1,2), 20))
  
  expect_error(summarise_and_label("input_df", "your_label", "rownum", TRUE, TRUE, TRUE, "none"), "data\\.frame")
  expect_error(summarise_and_label(input_df, 1, "rownum", TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label(input_df, list("your_label"), "rownum", TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label(input_df, "not_col", "rownum", TRUE, TRUE, TRUE, "none"), "column")
  expect_error(summarise_and_label(input_df, "your_label", 1, TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label(input_df, "your_label", list("rownum"), TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label(input_df, "your_label", c("rownum", "values"), TRUE, TRUE, TRUE, "none"), "only one")
  expect_error(summarise_and_label(input_df, "your_label", "not_col", TRUE, TRUE, TRUE, "none"), "column")
  expect_error(summarise_and_label(input_df, "your_label", "rownum", TRUE, TRUE, TRUE, "other"), "clean")
  expect_error(summarise_and_label(input_df, "your_label", "rownum", "T", TRUE, TRUE, "none"), "logical")
  expect_error(summarise_and_label(input_df, "your_label", "rownum", TRUE, "T", TRUE, "none"), "logical")
  expect_error(summarise_and_label(input_df, "your_label", "rownum", TRUE, TRUE, "T", "none"), "logical")
})

###############################################################################
test_that("multi summarise runs", {
  # arrange
  input_df = tibble::tibble(your_label = c(rep("z",50), rep("y",50)),
                            my_label = rep(c("a","b"), 50),
                            rownum = 1:100,
                            values = rep(c(0,0,NA,1,2), 20))

  # act
  group_by_list = list("your_label", "my_label")
  summarise_list = list("rownum", "values")
  actual_output = summarise_and_label_over_lists(df = input_df,
                                                 group_by_list,
                                                 summarise_list,
                                                 make_distinct = TRUE,
                                                 make_count = TRUE,
                                                 make_sum = TRUE,
                                                 clean = "none",
                                                 remove.na.from.groups = TRUE)
  expected_output = dplyr::bind_rows(
    summarise_and_label(df = input_df,
                        group_by_list[[1]],
                        summarise_list[[1]],
                        make_distinct = TRUE,
                        make_count = TRUE,
                        make_sum = TRUE,
                        clean = "none",
                        remove.na.from.groups = TRUE),
    summarise_and_label(df = input_df,
                        group_by_list[[2]],
                        summarise_list[[1]],
                        make_distinct = TRUE,
                        make_count = TRUE,
                        make_sum = TRUE,
                        clean = "none",
                        remove.na.from.groups = TRUE),
    summarise_and_label(df = input_df,
                        group_by_list[[1]],
                        summarise_list[[2]],
                        make_distinct = TRUE,
                        make_count = TRUE,
                        make_sum = TRUE,
                        clean = "none",
                        remove.na.from.groups = TRUE),
    summarise_and_label(df = input_df,
                        group_by_list[[2]],
                        summarise_list[[2]],
                        make_distinct = TRUE,
                        make_count = TRUE,
                        make_sum = TRUE,
                        clean = "none",
                        remove.na.from.groups = TRUE)
  )

  # assert
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})

test_that("multi summarise passes controls through",{
  # arrange
  input_df = tibble::tibble(your_label = c(rep("z",50), rep("y",50)),
                            my_label = rep(c("a","b"), 50),
                            rownum = 1:100,
                            values = rep(c(0,0,NA,1,2), 20))
  
  # act
  group_by_list = list("my_label")
  summarise_list = list("values")
  actual_output = summarise_and_label_over_lists(df = input_df,
                                                 group_by_list,
                                                 summarise_list,
                                                 make_distinct = TRUE,
                                                 make_count = TRUE,
                                                 make_sum = TRUE,
                                                 clean = "none",
                                                 remove.na.from.groups = TRUE)
  expected_output = summarise_and_label(df = input_df,
                                        group_by_list[[1]],
                                        summarise_list[[1]],
                                        make_distinct = TRUE,
                                        make_count = TRUE,
                                        make_sum = TRUE,
                                        clean = "none",
                                        remove.na.from.groups = TRUE)
  # assert
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  # act - make_distinct
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, FALSE, TRUE, TRUE, "none", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], FALSE, TRUE, TRUE, "none", TRUE)
  # assert
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  # act - make_count
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, TRUE, FALSE, TRUE, "none", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], TRUE, FALSE, TRUE, "none", TRUE)
  # assert
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  # act - make_sum
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, TRUE, TRUE, FALSE, "none", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], TRUE, TRUE, FALSE, "none", TRUE)
  # assert
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  # act - na.as.zero
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, TRUE, TRUE, TRUE, "na.as.zero", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], TRUE, TRUE, TRUE, "na.as.zero", TRUE)
  # assert
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  # act - zero.as.na
  actual_output = summarise_and_label_over_lists(input_df, group_by_list, summarise_list, TRUE, TRUE, TRUE, "zero.as.na", TRUE)
  expected_output = summarise_and_label(input_df, group_by_list[[1]], summarise_list[[1]], TRUE, TRUE, TRUE, "zero.as.na", TRUE)
  # assert
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  # act - remove.na.from.groups
  actual_output = summarise_and_label_over_lists(input_df, list("values"), list("rownum"), TRUE, TRUE, TRUE, "zero.as.na", FALSE)
  expected_output = summarise_and_label(input_df, "values", "rownum", TRUE, TRUE, TRUE, "zero.as.na", FALSE)
  # assert
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})

test_that("input checks stop execution", {
  input_df = tibble::tibble(your_label = c(rep("z",50), rep("y",50)),
                            my_label = rep(c("a","b"), 50),
                            rownum = 1:100,
                            values = rep(c(0,0,NA,1,2), 20))
  group_by_list = list("your_label", "my_label")
  summarise_list = list("rownum", "values")
  
  # expect_error(filter_to_limited_number_of_rows(rep(1:100)), "data\\.frame")
  expect_error(summarise_and_label_over_lists("input_df", group_by_list, summarise_list, TRUE, TRUE, TRUE, "none"), "data\\.frame")
  expect_error(summarise_and_label_over_lists(input_df, "group_by_list", summarise_list, TRUE, TRUE, TRUE, "none"), "list")
  expect_error(summarise_and_label_over_lists(input_df, group_by_list, "summarise_list", TRUE, TRUE, TRUE, "none"), "list")
  expect_error(summarise_and_label_over_lists(input_df, list(), summarise_list, TRUE, TRUE, TRUE, "none"), "at least one")
  expect_error(summarise_and_label_over_lists(input_df, group_by_list, list(), TRUE, TRUE, TRUE, "none"), "at least one")
  expect_error(summarise_and_label_over_lists(input_df, list("my_label", 2), summarise_list, TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label_over_lists(input_df, group_by_list, list("rownum", 3), TRUE, TRUE, TRUE, "none"), "character")
  expect_error(summarise_and_label_over_lists(input_df, list("my_label", "not col"), summarise_list, TRUE, TRUE, TRUE, "none"), "column")
  expect_error(summarise_and_label_over_lists(input_df, group_by_list, list("rownum", "not col"), TRUE, TRUE, TRUE, "none"), "column")
})

test_that("different input types merge", {
  input_df = tibble::tibble(
    c1 = c("a","b","a","b"),
    c2 = c(1.0, 1.0, 2.5, 2.5),
    c3 = c(9, 8, 7, 6)
  )
  group_by_list = list("c1", "c2")
  summarise_list = list("c3")
  
  expected_output = tibble::tibble(
    col01 = c("c1", "c1", "c2", "c2"),
    val01 = c("a", "b", "1", "2.5"),
    summarised_var = c("c3", "c3", "c3", "c3"),
    sum = c(16, 14, 17, 13)
  )
  
  actual_output = summarise_and_label_over_lists(
    df = input_df,
    group_by_list = group_by_list,
    summarise_list = summarise_list,
    make_distinct = FALSE,
    make_count = FALSE,
    make_sum = TRUE
  )
  
  expect_true(all_equal(actual_output, expected_output, ignore_row_order = TRUE, ignore_col_order = TRUE))
})

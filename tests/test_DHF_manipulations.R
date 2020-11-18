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

#' Testing the following functions that manipulate tables
#'
#' union_all(table_a,table_b, list_of_columns)
#' pivot_table(input_tbl, label_column, value_column, aggregator = "SUM")
#' collapse_indicator_columns(input_tbl, prefix, yes_value, label = NA)
#'
context("dbplyr helpers - manipulations")

test_that("union row-binds", {
  # arrange
  head_table_name <- add_delimiters(paste0("test", floor(runif(1, 1000000, 9999999))), "[]")
  tail_table_name <- add_delimiters(paste0("test", floor(runif(1, 1000000, 9999999))), "[]")
  data(cars)
  head_cars <- head(cars)
  tail_cars <- tail(cars)
  # act - load and union
  db_con <- create_database_connection(database = our_db)
  head_table <- copy_r_to_sql(db_con, our_db, our_schema, head_table_name, head_cars)
  tail_table <- copy_r_to_sql(db_con, our_db, our_schema, tail_table_name, tail_cars)
  unioned_table <- union_all(head_table, tail_table, colnames(cars)) %>% collect()
  rowbound_table <- rbind(head_cars, tail_cars)
  # act - delete & tidy up
  delete_table(db_con, our_db, our_schema, head_table_name, mode = "table")
  delete_table(db_con, our_db, our_schema, tail_table_name, mode = "table")
  close_database_connection(db_con)
  # assert
  expect_true(all_equal(unioned_table, rowbound_table, ignore_row_order = TRUE))
})

test_that("pivot replicates tidyr::spread", {
  # arrange
  table_name <- add_delimiters(paste0("test", floor(runif(1, 1000000, 9999999))), "[]")
  in_data_table <- data.frame(
    people = c("bob", "alice", "bob", "alice"),
    labels = c("age", "age", "height", "height"),
    values = c(10, 12, 150, 160),
    stringsAsFactors = FALSE
  )
  out_data_table <- data.frame(
    people = c("bob", "alice"),
    age = c(10, 12),
    height = c(150, 160),
    stringsAsFactors = FALSE
  )
  # act - load and pivot
  db_con <- create_database_connection(database = our_db)
  sql_table <- copy_r_to_sql(db_con, our_db, our_schema, table_name, in_data_table)
  pivoted_table <- sql_table %>%
    pivot_table(label_column = "labels", value_column = "values", aggregator = "SUM") %>%
    collect()
  spread_table <- tidyr::spread(in_data_table, labels, values)
  delete_table(db_con, our_db, our_schema, table_name)
  close_database_connection(db_con)
  # assert
  expect_true(all_equal(pivoted_table, out_data_table, ignore_row_order = TRUE, ignore_col_order = TRUE))
  expect_true(all_equal(pivoted_table, spread_table, ignore_row_order = TRUE, ignore_col_order = TRUE))
})

test_that("collapse indicator columns gives expected errors and warnings", {
  input_tbl <- data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 1, 0),
    v_c = c(0, 1, 1)
  )
  prefix <- "v_"
  label <- "v"
  yes_value <- 1

  expect_error(collapse_indicator_columns(123, prefix, yes_value, label))
  expect_error(collapse_indicator_columns(input_tbl, 123, yes_value, label))
  expect_error(collapse_indicator_columns(input_tbl, prefix, yes_value, 123))
  expect_error(collapse_indicator_columns("input_tbl", prefix, yes_value, label))

  expect_warning(collapse_indicator_columns(input_tbl, "w", yes_value, label))
})


test_that("collapse indicator columns runs for local data frames", {
  input_tbl <- data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 1, 0),
    v_c = c(0, 1, 1)
  )
  prefix <- "v_"
  label <- "v"
  yes_value <- 1

  expected_output_tbl <- data.frame(id = c(1, 2, 3), v = c("a", "b", "c"), stringsAsFactors = FALSE)
  actual_output_tbl <- collapse_indicator_columns(input_tbl, prefix, yes_value, label)

  expect_true(all_equal(actual_output_tbl, expected_output_tbl, ignore_row_order = TRUE, ignore_col_order = TRUE))
})

test_that("collapse indicator columns runs for remote data frames", {
  # arrange
  input_tbl <- data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 1, 0),
    v_c = c(0, 1, 1)
  )
  prefix <- "v_"
  label <- "v"
  yes_value <- 1

  expected_output_tbl <- data.frame(id = c(1, 2, 3), v = c("a", "b", "c"), stringsAsFactors = FALSE)

  # act
  table_name <- add_delimiters(paste0("test", floor(runif(1, 1000000, 9999999))), "[]")
  db_con <- create_database_connection(database = our_db)
  sql_table <- copy_r_to_sql(db_con, our_db, our_schema, table_name, input_tbl)
  actual_output_tbl <- sql_table %>%
    collapse_indicator_columns(prefix, yes_value, label) %>%
    collect()
  delete_table(db_con, our_db, our_schema, table_name)
  close_database_connection(db_con)

  # assert
  expect_true(all_equal(actual_output_tbl, expected_output_tbl, ignore_row_order = TRUE, ignore_col_order = TRUE))
})

test_that("collapse indicator columns runs for local data frames with multiple yes values", {
  input_tbl <- data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 1, 0),
    v_c = c(0, 2, 2)
  )
  prefix <- "v_"
  label <- "v"
  yes_value <- c(1, 2)

  expected_output_tbl <- data.frame(id = c(1, 2, 3), v = c("a", "b", "c"), stringsAsFactors = FALSE)
  actual_output_tbl <- collapse_indicator_columns(input_tbl, prefix, yes_value, label)

  expect_true(all_equal(actual_output_tbl, expected_output_tbl, ignore_row_order = TRUE, ignore_col_order = TRUE))
})

test_that("collapse indicator columns runs for remote data frames with multiple yes values", {
  # arrange
  input_tbl <- data.frame(
    id = c(1, 2, 3),
    v_a = c(1, 0, 0),
    v_b = c(0, 1, 0),
    v_c = c(0, 2, 2)
  )
  prefix <- "v_"
  label <- "v"
  yes_value <- c(1, 2)

  expected_output_tbl <- data.frame(id = c(1, 2, 3), v = c("a", "b", "c"), stringsAsFactors = FALSE)

  # act
  table_name <- add_delimiters(paste0("test", floor(runif(1, 1000000, 9999999))), "[]")
  db_con <- create_database_connection(database = our_db)
  sql_table <- copy_r_to_sql(db_con, our_db, our_schema, table_name, input_tbl)
  actual_output_tbl <- sql_table %>%
    collapse_indicator_columns(prefix, yes_value, label) %>%
    collect()
  delete_table(db_con, our_db, our_schema, table_name)
  close_database_connection(db_con)

  # assert
  expect_true(all_equal(actual_output_tbl, expected_output_tbl, ignore_row_order = TRUE, ignore_col_order = TRUE))
})

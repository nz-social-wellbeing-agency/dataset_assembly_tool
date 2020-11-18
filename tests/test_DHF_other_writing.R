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

#' Testing the following functions that do other types of writting
#'
#' write_to_database(input_tbl, db_connection, schema, tbl_name, OVERWRITE = FALSE)
#' create_table(db_connection, schema, tbl_name, named_list_of_columns, OVERWRITE = FALSE)
#' create_view(tbl_name, db_connection, schema, view_name, OVERWRITE = FALSE)
#' append_database_table(db_connection, schema, tbl_name, list_of_columns, table_to_append)
#'
context("dbplyr helpers - other writings")

## setup ----

table_name <- add_delimiters(paste0("test_tbl", floor(runif(1, 1000000, 9999999))), "[]")
view_name <- add_delimiters(paste0("test_view", floor(runif(1, 1000000, 9999999))), "[]")
data(cars)

## tests ----

test_that("sql accepts creation and appending", {
  # arrange
  table_name1 <- add_delimiters(paste0("test_tbl", floor(runif(1, 1000000, 9999999))), "[]")
  table_name2 <- add_delimiters(paste0("test_tbl", floor(runif(1, 1000000, 9999999))), "[]")
  named_list_of_columns <- list(number = "[int] NOT NULL", date = "[date] NOT NULL", character = "[varchar](25) NULL")
  table_data <- data.frame(
    number = c(1, 2, 3),
    date = c("2000-02-29", "2001-01-01", "2003-12-31"),
    character = c("a", "b", "c"), stringsAsFactors = FALSE
  )
  # act - blank table
  db_con <- create_database_connection(database = our_db)
  create_table(db_con, our_db, our_schema, table_name1, named_list_of_columns)
  table_created_in_sql <- table_or_view_exists_in_db(db_con, our_db, our_schema, table_name1)
  new_table <- create_access_point(db_con, our_db, our_schema, table_name1)
  new_table_row_count <- new_table %>% ungroup() %>% summarise(num = dplyr::n()) %>% collect() %>% unlist(use.names = FALSE)
  # act - append
  remote_table <- copy_r_to_sql(db_con, our_db, our_schema, table_name2, table_data)
  remote_table_row_count <- remote_table %>% summarise(num = n()) %>% collect() %>% unlist(use.names = FALSE)
  append_database_table(db_con, our_db, our_schema, table_name1, colnames(table_data), remote_table)
  appended_table_row_count <- new_table %>% summarise(num = n()) %>% collect() %>% unlist(use.names = FALSE)
  # act - delete & tidy up
  delete_table(db_con, our_db, our_schema, table_name1, mode = "table")
  table1_deleted_from_sql <- !table_or_view_exists_in_db(db_con, our_db, our_schema, table_name1)
  delete_table(db_con, our_db, our_schema, table_name2, mode = "table")
  table2_deleted_from_sql <- !table_or_view_exists_in_db(db_con, our_db, our_schema, table_name2)
  close_database_connection(db_con)
  # assert
  expect_true(table_created_in_sql)
  expect_equal(new_table_row_count, 0)
  expect_equal(appended_table_row_count, nrow(table_data))
  expect_equal(remote_table_row_count, nrow(table_data))
  expect_true(table1_deleted_from_sql)
  expect_true(table2_deleted_from_sql)
})

test_that("new tables can be written", {
  copied_table_name <- add_delimiters(paste0("test_tbl", floor(runif(1, 1000000, 9999999))), "[]")
  written_table_name <- add_delimiters(paste0("test_tbl", floor(runif(1, 1000000, 9999999))), "[]")
  # act - copy in
  db_con <- create_database_connection(database = our_db)
  copied_table <- copy_r_to_sql(db_con, our_db, our_schema, copied_table_name, cars)
  table_copied_to_sql <- table_or_view_exists_in_db(db_con, our_db, our_schema, copied_table_name)
  # act - rewrite
  written_table <- write_to_database(copied_table, db_con, our_db, our_schema, written_table_name)
  table_written_to_sql <- table_or_view_exists_in_db(db_con, our_db, our_schema, written_table_name)
  # act - delete & tidy up
  delete_table(db_con, our_db, our_schema, copied_table_name, mode = "table")
  copied_deleted_from_sql <- !table_or_view_exists_in_db(db_con, our_db, our_schema, copied_table_name)
  delete_table(db_con, our_db, our_schema, written_table_name, mode = "table")
  written_deleted_from_sql <- !table_or_view_exists_in_db(db_con, our_db, our_schema, written_table_name)
  close_database_connection(db_con)
  # assert
  expect_true(table_copied_to_sql)
  expect_true(table_written_to_sql)
  expect_true(copied_deleted_from_sql)
  expect_true(written_deleted_from_sql)
})

test_that("views can be created and deleted", {
  # act - view
  db_con <- create_database_connection(database = "IDI_UserCode")
  test_table <- copy_r_to_sql(db_con, our_db, our_schema, table_name, cars)
  test_view <- create_view(test_table, db_con, our_usercode, our_schema, view_name)
  view_in_sql <- table_or_view_exists_in_db(db_con, our_usercode, our_schema, view_name)
  # act - removal
  delete_table(db_con, our_db, our_schema, table_name, mode = "table")
  delete_table(db_con, our_usercode, our_schema, view_name, mode = "view")
  view_deleted_from_sql <- !table_or_view_exists_in_db(db_con, our_usercode, our_schema, view_name)
  close_database_connection(db_con)
  # assert
  expect_true(view_in_sql)
  expect_true(view_deleted_from_sql)
})

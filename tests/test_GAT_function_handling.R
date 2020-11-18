###############################################################################
#' Description: Automated tests for general assembly tool.
#'
#' Input: general_assembly_tool.R
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
#' 2019-12-16 SA v0
#' #############################################################################

#' Testing the following functions that handle text for SQL
#'
#' validate_control_tables(population_table, measure_table)
#' validate_database_tables(input_table)
#'
context("assembler - tool function handling pre-checks")

## setup - read in and prepare control file inputs ------------------------------------------------

tmp1 <- readxl::read_xlsx("./input1_population_and_period.xlsx")
tmp1[] <- lapply(tmp1, as.character)
colnames(tmp1) <- tolower(colnames(tmp1))
tmp1[, "schema_name"] <- our_schema

tmp2 <- readxl::read_xlsx("./input2_measures.xlsx")
tmp2[] <- lapply(tmp2, as.character)
colnames(tmp2) <- tolower(colnames(tmp2))
tmp2[, "schema_name"] <- our_schema

## testing validate_control_tables function -------------------------------------------------------

test_that("accepted controls are validated", {
  expect_output(validate_control_tables(tmp1, tmp2, control_verbose = "all"))
  expect_silent(validate_control_tables(tmp1, tmp2, control_verbose = "none"))
})

test_that("swapped control tables errors", {
  expect_error(validate_control_tables(tmp2, tmp1, control_verbose = "none"))
})

test_that("wrong columns errors", {
  tmp2b <- rename(tmp2, wrong = table_name)
  expect_error(validate_control_tables(tmp1, tmp2b, control_verbose = "none"))
})

test_that("incorrect table contents gives errors and warnings", {
  # testthat package does not have a function for expect_error_and_warning
  # expect_error alone fails as there are also warnings
  # expect_warning alone fails as there are also errors
  #
  # expect_condition with class = warning seems to be the best solution for warning
  # expect_error with warnings suppressed seems to be the best solution for errors
  tmp1b <- tmp1; tmp1b[1, 1] <- "error causing"
  expect_condition(validate_control_tables(tmp1b, tmp2, control_verbose = "none"), class = "warning")
  expect_error(suppressWarnings(validate_control_tables(tmp1b, tmp2, control_verbose = "none")))
})

test_that("missing values in table errors", {
  tmp1b <- tmp1; tmp1b[1, 1] <- NA
  tmp2b <- tmp2; tmp2b[1, 5] <- NA
  expect_condition(validate_control_tables(tmp1b, tmp2, control_verbose = "none"), class = "warning")
  expect_condition(validate_control_tables(tmp1, tmp2b, control_verbose = "none"), class = "warning")
  expect_error(suppressWarnings(validate_control_tables(tmp1b, tmp2, control_verbose = "none")))
  expect_error(suppressWarnings(validate_control_tables(tmp1, tmp2b, control_verbose = "none")))
})

## set up database - write tables to SQL for testing ----------------------------------------------

# load test tables from Excel into R
accidents <- readxl::read_xlsx("./accidents.xlsx", col_types = c("numeric", "date", "text"))
benefit_payment <- readxl::read_xlsx("./benefit_payment.xlsx", col_types = c("numeric", "date", "date", "numeric"))
project_population <- readxl::read_xlsx("./project_population.xlsx", col_types = c("numeric", "date", "date", "text"))

for (col in colnames(accidents)) {
  if (is.character(accidents[[1, col]])) {
    accidents[, col] <- gsub("[\"']", "", accidents[[col]])
  }
}
for (col in colnames(benefit_payment)) {
  if (is.character(benefit_payment[[1, col]])) {
    benefit_payment[, col] <- gsub("[\"']", "", benefit_payment[[col]])
  }
}
for (col in colnames(project_population)) {
  if (is.character(project_population[[1, col]])) {
    project_population[, col] <- gsub("[\"']", "", project_population[[col]])
  }
}

# load test tables from R into SQL
db_con <- create_database_connection(database = our_db)
copy_r_to_sql(db_con, our_db, our_schema, "[tmp_accidents]", accidents, OVERWRITE = TRUE)
copy_r_to_sql(db_con, our_db, our_schema, "[tmp_benefit_payment]", benefit_payment, OVERWRITE = TRUE)
copy_r_to_sql(db_con, our_db, our_schema, "[tmp_project_population]", project_population, OVERWRITE = TRUE)
close_database_connection(db_con)

## testing validate_database_tables function ------------------------------------------------------

test_that("validation against database accepts correct input", {
  # validate accepts correct input
  expect_silent(validate_database_tables(tmp1, control_verbose = "none", output_database = "[IDI_Sandpit]"))
  expect_silent(validate_database_tables(tmp2, control_verbose = "none", output_database = "[IDI_Sandpit]"))
  expect_output(validate_database_tables(tmp1, control_verbose = "all", output_database = "[IDI_Sandpit]"))
  expect_output(validate_database_tables(tmp2, control_verbose = "all", output_database = "[IDI_Sandpit]"))
})

test_that("validation against database rejects incorrect input", {
  # break input tables
  tmp1b <- tmp1; tmp1b[1, 1] <- "[db that does not exist]"
  tmp2b <- tmp2; tmp2b[1, 5] <- "[col that does not exist]"
  # validate rejects incorrect input
  expect_error(validate_database_tables(tmp1b, control_verbose = "none", output_database = "[IDI_Sandpit]"))
  expect_error(validate_database_tables(tmp2b, control_verbose = "none", output_database = "[IDI_Sandpit]"))
})

test_that("numeric operations fail on non-numeric columns", {
  # introduce flaw, last column is numeric but read as text
  benefit_payment_flaw <- readxl::read_xlsx("./benefit_payment.xlsx", col_types = c("numeric", "date", "date", "text"))
  for (col in colnames(benefit_payment)) {
    if (is.character(benefit_payment[[1, col]])) {
      benefit_payment[, col] <- gsub("[\"']", "", benefit_payment[[col]])
    }
  }

  db_con <- create_database_connection(database = our_db)
  copy_r_to_sql(db_con, our_db, our_schema, "[tmp_benefit_payment_flaw]", benefit_payment_flaw, OVERWRITE = TRUE)
  close_database_connection(db_con)

  # point measure table to flawed table
  tmp2b <- tmp2
  tmp2b[tmp2b$table_name == "[tmp_benefit_payment]", "table_name"] <- "[tmp_benefit_payment_flaw]"

  expect_error(validate_database_tables(tmp2b, control_verbose = "none", output_database = "[IDI_Sandpit]"))
  expect_silent(validate_database_tables(tmp2, control_verbose = "none", output_database = "[IDI_Sandpit]"))

  # tidy up
  db_con <- create_database_connection(database = our_db)
  delete_table(db_con, our_db, our_schema, "[tmp_benefit_payment_flaw]")
  close_database_connection(db_con)
})

## testing assemble_output_table function ---------------------------------------------------------

test_that("assembly is occurs", {
  # make output
  expect_output(assemble_output_table(tmp1, tmp2, our_db, our_schema, "[tmp_output]",
    control_development_mode = FALSE,
    control_overwrite_output_table = TRUE,
    control_verbose = "all"
  ))
  expect_silent(assemble_output_table(tmp1, tmp2, our_db, our_schema, "[tmp_output]",
    control_development_mode = FALSE,
    control_overwrite_output_table = TRUE,
    control_verbose = "none"
  ))

  # retrieve output table
  db_con <- create_database_connection(database = our_db)
  tmp_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]")
  tmp_output_size <- tmp_output %>% summarise(num = n()) %>% collect() %>% unlist(use.names = FALSE)
  close_database_connection(db_con)
  # output exists
  expect_true(tmp_output_size > 1)
})

test_that("assembly is correct", {
  # load expected output files
  output_example <- readxl::read_xlsx("./output_example.xlsx",
    col_types = c(
      "numeric", "text", "date", "date", "text", "text", "numeric",
      "text", "text", "text"
    )
  )
  # load actual output
  db_con <- create_database_connection(database = our_db)
  actual_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]") %>% collect()
  close_database_connection(db_con)

  # standardise
  for (col in colnames(output_example)) {
    if (is.character(output_example[[1, col]])) {
      output_example[, col] <- gsub("[\"']", "", output_example[[col]])
    }
  }
  colnames(output_example) <- tolower(colnames(output_example))
  output_example <- output_example %>%
    filter(value_measure != 0) %>%
    mutate(value_measure = round(value_measure, 1))

  for (col in colnames(actual_output)) {
    if (is.character(actual_output[[1, col]])) {
      actual_output[, col] <- gsub("[\"']", "", actual_output[[col]])
    }
  }
  colnames(actual_output) <- tolower(colnames(actual_output))
  actual_output <- actual_output %>%
    filter(value_measure != 0) %>%
    mutate(value_measure = round(value_measure, 1))

  output_example$summary_period_start_date <- as.Date(output_example$summary_period_start_date)
  output_example$summary_period_end_date <- as.Date(output_example$summary_period_end_date)

  # tests
  expect_true(all(colnames(actual_output) %in% colnames(output_example)))

  output_example <- output_example %>% select(colnames(actual_output))

  expect_true(all_equal(output_example, actual_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})

## tidy up database - remove tables from SQL after testing ----------------------------------------
db_con <- create_database_connection(database = our_db)
delete_table(db_con, our_db, our_schema, "[tmp_accidents]")
delete_table(db_con, our_db, our_schema, "[tmp_benefit_payment]")
delete_table(db_con, our_db, our_schema, "[tmp_project_population]")
delete_table(db_con, our_db, our_schema, "[tmp_output]")
close_database_connection(db_con)

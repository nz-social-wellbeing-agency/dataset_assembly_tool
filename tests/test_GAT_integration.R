###############################################################################
#' Description: Automated tests for entire general assembly tool.
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
#' 2019-12-20 SA v0
#' #############################################################################

context("assembler - tool integration testing")

## setup - repeated activities as functions -------------------------------------------------------

reset_output <- function() {
  db_con <- create_database_connection(database = our_db)
  delete_table(db_con, our_db, our_schema, "[tmp_output]")
  close_database_connection(db_con)
}

output_exists <- function() {
  db_con <- create_database_connection(database = our_db)
  tmp_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]")
  tmp_output_size <- tmp_output %>% summarise(num = n()) %>% collect() %>% unlist(use.names = FALSE)
  close_database_connection(db_con)
  return(tmp_output_size > 1)
}

standardise_table <- function(table) {
  for (col in colnames(table)) {
    if (is.character(table[[1, col]])) {
      table[, col] <- gsub("[\"']", "", table[[col]])
    }
  }
  colnames(table) <- tolower(colnames(table))
  table <- table %>%
    filter(value_measure != 0) %>%
    mutate(value_measure = round(value_measure, 1))
  return(table)
}

## setup - read in and prepare control file inputs ------------------------------------------------

tmp1 <- readxl::read_xlsx("./input1_population_and_period.xlsx")
tmp1[] <- lapply(tmp1, as.character)
colnames(tmp1) <- tolower(colnames(tmp1))
tmp1[, "schema_name"] <- our_schema
xlsx::write.xlsx(as.data.frame(tmp1), "./input1_population_and_period_alt.xlsx", row.names = FALSE)

tmp2 <- readxl::read_xlsx("./input2_measures.xlsx")
tmp2[] <- lapply(tmp2, as.character)
colnames(tmp2) <- tolower(colnames(tmp2))
tmp2[, "schema_name"] <- our_schema
xlsx::write.xlsx(as.data.frame(tmp2), "./input2_measures_alt.xlsx", row.names = FALSE)

# load expected output files
output_example <- readxl::read_xlsx("./output_example.xlsx",
  col_types = c(
    "numeric", "text", "date", "date", "text", "text", "numeric",
    "text", "text", "text"
  )
)
# standardise
output_example <- standardise_table(output_example)
output_example$summary_period_start_date <- as.Date(output_example$summary_period_start_date)
output_example$summary_period_end_date <- as.Date(output_example$summary_period_end_date)

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

## integration base test --------------------------------------------------------------------------

test_that("tool runs", {
  reset_output()

  general_data_assembly_tool(
    input_population_and_period_table = tmp1,
    input_measures_to_assemble_table = tmp2,
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "none", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = FALSE,
    control_run_checks_only = FALSE,
    control_skip_pre_checks = FALSE
  )

  expect_true(output_exists())

  # load actual output
  db_con <- create_database_connection(database = our_db)
  actual_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]") %>% collect()
  close_database_connection(db_con)
  actual_output <- standardise_table(actual_output)

  # columns correct
  expect_true(all(colnames(actual_output) %in% colnames(output_example)))
  output_example <- output_example %>% select(colnames(actual_output))
  # contents correct
  expect_true(all_equal(output_example, actual_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})

## integration variation tests --------------------------------------------------------------------

test_that("tool accepts string inputs", {
  reset_output()

  general_data_assembly_tool(
    input_population_and_period_table = "./input1_population_and_period_alt.xlsx",
    input_measures_to_assemble_table = "./input2_measures_alt.xlsx",
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "none", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = FALSE,
    control_run_checks_only = FALSE,
    control_skip_pre_checks = FALSE
  )

  expect_true(output_exists())

  # load actual output
  db_con <- create_database_connection(database = our_db)
  actual_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]") %>% collect()
  close_database_connection(db_con)
  actual_output <- standardise_table(actual_output)

  # columns correct
  expect_true(all(colnames(actual_output) %in% colnames(output_example)))
  output_example <- output_example %>% select(colnames(actual_output))
  # contents correct
  expect_true(all_equal(output_example, actual_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
})

test_that("tool appends", {
  reset_output()

  general_data_assembly_tool(
    input_population_and_period_table = tmp1,
    input_measures_to_assemble_table = tmp2,
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "none", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = FALSE,
    control_run_checks_only = FALSE,
    control_skip_pre_checks = FALSE
  )

  expect_true(output_exists())

  db_con <- create_database_connection(database = our_db)
  tmp_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]")
  tmp_output_size <- tmp_output %>% summarise(num = n()) %>% collect() %>% unlist(use.names = FALSE)
  close_database_connection(db_con)

  general_data_assembly_tool(
    input_population_and_period_table = tmp1,
    input_measures_to_assemble_table = tmp2,
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "none", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = FALSE,
    control_run_checks_only = FALSE,
    control_skip_pre_checks = FALSE
  )

  expect_true(output_exists())

  db_con <- create_database_connection(database = our_db)
  tmp_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]")
  tmp_output_size2 <- tmp_output %>% summarise(num = n()) %>% collect() %>% unlist(use.names = FALSE)
  close_database_connection(db_con)

  # appended doubles size
  expect_true(tmp_output_size * 2 == tmp_output_size2)
})

test_that("tool overwrites", {
  reset_output()

  general_data_assembly_tool(
    input_population_and_period_table = tmp1,
    input_measures_to_assemble_table = tmp2,
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "none", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = FALSE,
    control_run_checks_only = FALSE,
    control_skip_pre_checks = FALSE
  )

  expect_true(output_exists())

  db_con <- create_database_connection(database = our_db)
  tmp_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]")
  tmp_output_size <- tmp_output %>% summarise(num = n()) %>% collect() %>% unlist(use.names = FALSE)
  close_database_connection(db_con)

  general_data_assembly_tool(
    input_population_and_period_table = tmp1,
    input_measures_to_assemble_table = tmp2,
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "none", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = TRUE,
    control_run_checks_only = FALSE,
    control_skip_pre_checks = FALSE
  )

  expect_true(output_exists())

  db_con <- create_database_connection(database = our_db)
  tmp_output <- create_access_point(db_con, our_db, our_schema, "[tmp_output]")
  tmp_output_size2 <- tmp_output %>% summarise(num = n()) %>% collect() %>% unlist(use.names = FALSE)
  close_database_connection(db_con)

  # appended doubles size
  expect_true(tmp_output_size == tmp_output_size2)
})

test_that("runtime info is (not) displayed", {
  expect_output(general_data_assembly_tool(
    input_population_and_period_table = "./input1_population_and_period_alt.xlsx",
    input_measures_to_assemble_table = "./input2_measures_alt.xlsx",
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "default", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = FALSE,
    control_run_checks_only = FALSE,
    control_skip_pre_checks = FALSE
  ))

  expect_output(general_data_assembly_tool(
    input_population_and_period_table = "./input1_population_and_period_alt.xlsx",
    input_measures_to_assemble_table = "./input2_measures_alt.xlsx",
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "all", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = FALSE,
    control_run_checks_only = FALSE,
    control_skip_pre_checks = FALSE
  ))

  # disabled as read-table function print a new line
  # expect_silent(general_data_assembly_tool(
  #   input_population_and_period_table = "./input1_population_and_period_alt.xlsx",
  #   input_measures_to_assemble_table = "./input2_measures_alt.xlsx",
  #   output_database = our_db,
  #   output_schema = our_schema,
  #   output_table = "[tmp_output]",
  #   control_development_mode = FALSE,
  #   control_verbose = "none", # {"all", "details", "heading", "none", "default"}
  #   control_overwrite_output_table = FALSE,
  #   control_run_checks_only = FALSE,
  #   control_skip_pre_checks = FALSE
  # ))
})

test_that("only checks can be run", {
  reset_output()

  general_data_assembly_tool(
    input_population_and_period_table = tmp1,
    input_measures_to_assemble_table = tmp2,
    output_database = our_db,
    output_schema = our_schema,
    output_table = "[tmp_output]",
    control_development_mode = FALSE,
    control_verbose = "none", # {"all", "details", "heading", "none", "default"}
    control_overwrite_output_table = FALSE,
    control_run_checks_only = TRUE,
    control_skip_pre_checks = FALSE
  )

  expect_error(output_exists())
})

## tidy up database - remove tables from SQL after testing ----------------------------------------
db_con <- create_database_connection(database = our_db)
delete_table(db_con, our_db, our_schema, "[tmp_accidents]")
delete_table(db_con, our_db, our_schema, "[tmp_benefit_payment]")
delete_table(db_con, our_db, our_schema, "[tmp_project_population]")
delete_table(db_con, our_db, our_schema, "[tmp_output]")
close_database_connection(db_con)

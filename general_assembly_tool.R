###############################################################################
#' Description: Data assembly tool
#'
#' Input: Two control files (1) population definition, (2) measures for analysis
#'
#' Output: Summarised measures for each population
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: utility_functions.R, general_assembly_tool_functions.R already sourced
#'
#' Notes:
#' - Uses code folding by headers (Alt + O to collapse all)
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-11-18 SA v2 for release
#' 2020-01-06 SA v1
#' 2019-11-26 SA v0
#' #############################################################################

## source ----

required_packages <- c("dplyr", "dbplyr", "odbc", "DBI", "readxl", "glue")
for (pkg in required_packages) {
  if (!pkg %in% installed.packages()) {
    warning(sprintf("package \"%s\" not installed", pkg))
  }
}

## master function --------------------------------------------------------------------------------
#'
general_data_assembly_tool <- function(
                                       input_population_and_period_table,
                                       input_measures_to_assemble_table,
                                       output_database,
                                       output_schema,
                                       output_table,
                                       control_development_mode,
                                       control_verbose = "default", # {"all", "details", "heading", "none", "default"}
                                       control_overwrite_output_table = FALSE,
                                       control_run_checks_only = FALSE,
                                       control_skip_pre_checks = FALSE) {
  #### setup ----
  if (control_verbose == "default") {
    control_verbose <- ifelse(control_development_mode, "all", "heading")
  }

  run_time_inform_user("general data assembly tool begun", context = "heading", print_level = control_verbose)

  #### load tables if file path provided ----
  if (is.character(input_population_and_period_table) && file.exists(input_population_and_period_table)) {
    input_population_and_period_table <- read_table_file(input_population_and_period_table)
  }
  if (is.character(input_measures_to_assemble_table) && file.exists(input_measures_to_assemble_table)) {
    input_measures_to_assemble_table <- read_table_file(input_measures_to_assemble_table)
  }

  #### all input is text ----
  input_population_and_period_table[] <- lapply(input_population_and_period_table, as.character)
  input_measures_to_assemble_table[] <- lapply(input_measures_to_assemble_table, as.character)

  #### validate input tables ----
  if (is.tbl(input_population_and_period_table)) {
    input_population_and_period_table <- as.data.frame(input_population_and_period_table)
  }
  assert(is.data.frame(input_population_and_period_table), "population and period control input invalid")

  if (is.tbl(input_measures_to_assemble_table)) {
    input_measures_to_assemble_table <- as.data.frame(input_measures_to_assemble_table)
  }
  assert(is.data.frame(input_measures_to_assemble_table), "measures control input invalid")

  #### validate output and controls ----
  no_special_characters(output_database)
  no_special_characters(output_schema)
  no_special_characters(output_table)
  assert(is_delimited(output_database, "[]"), "output database must be delimited")
  assert(is_delimited(output_schema, "[]"), "output schema must be delimited")
  assert(is_delimited(output_table, "[]"), "output table must be delimited")

  assert(is.logical(control_development_mode), "development mode control must be logical")
  assert(control_verbose %in% c("all", "details", "heading", "none"), "invalid verbose display control")
  assert(is.logical(control_overwrite_output_table), "overwrite control must be logical")
  assert(is.logical(control_skip_pre_checks), "skip checks control must be logical")

  #### validate control tables ----
  colnames(input_population_and_period_table) <- tolower(colnames(input_population_and_period_table))
  colnames(input_measures_to_assemble_table) <- tolower(colnames(input_measures_to_assemble_table))

  if (!control_skip_pre_checks) {
    validate_control_tables(input_population_and_period_table, input_measures_to_assemble_table, control_verbose)
    run_time_inform_user("controls and inputs validated", context = "heading", print_level = control_verbose)
  }

  #### keep only required columns ----
  population_required_cols <- c(
    "database_name", "schema_name", "table_name", "identity_column",
    "label_identity", "summary_period_start_date", "summary_period_end_date", "label_summary_period"
  )
  measure_required_cols <- c(
    "database_name", "schema_name", "table_name", "identity_column",
    "measure_period_start_date", "measure_period_end_date", "label_measure", "value_measure",
    "measure_summarised_by", "proportional"
  )
  input_population_and_period_table <- select(input_population_and_period_table, all_of(population_required_cols))
  input_measures_to_assemble_table <- select(input_measures_to_assemble_table, all_of(measure_required_cols))

  #### validate database tables ----
  if (!control_skip_pre_checks) {
    validate_database_tables(input_population_and_period_table, control_verbose, output_database)
    validate_database_tables(input_measures_to_assemble_table, control_verbose, output_database)
    run_time_inform_user("database contents validated", context = "heading", print_level = control_verbose)
  }

  #### create output table ----
  if (!control_run_checks_only) {
    assemble_output_table(
      input_population_and_period_table, input_measures_to_assemble_table,
      output_database, output_schema, output_table,
      control_development_mode, control_overwrite_output_table, control_verbose
    )
    run_time_inform_user("output table created", context = "heading", print_level = control_verbose)
  }

  #### finish ----
  run_time_inform_user("general data assembly tool ended", context = "heading", print_level = control_verbose)
}

## validate controls ------------------------------------------------------------------------------
#'
#' Check both input control files have required columns
#' and that these columns have the correctly delimied types.
#'
validate_control_tables <- function(population_table, measure_table, control_verbose) {
  any_check_failed <- FALSE
  #### population and period ----

  # acceptable and default values
  column_reqs <- data.frame(
    Database_name              = c("sql only", TRUE, FALSE),
    Schema_name                = c("sql only", TRUE, FALSE),
    Table_name                 = c("sql only", TRUE, FALSE),
    Identity_column            = c("sql or txt", TRUE, TRUE),
    Label_Identity             = c("sql or txt", TRUE, TRUE),
    Summary_period_start_date  = c("sql or txt", TRUE, TRUE),
    Summary_period_end_date    = c("sql or txt", TRUE, TRUE),
    Label_summary_period       = c("sql or txt", TRUE, TRUE),
    stringsAsFactors = FALSE,
    row.names = c("accepts", "sql", "txt")
  )
  # required columns
  colnames(column_reqs) <- tolower(colnames(column_reqs))
  for (col in colnames(column_reqs)) {
    assert(col %in% colnames(population_table), sprintf("column %s missing from population input table", col))
  }

  # required values
  for (col in colnames(column_reqs)) {
    for (row in 1:nrow(population_table)) {
      # not missing
      not_na_check <- !is.na(population_table[[row, col]])
      # required delimiters
      sql_check <- not_na_check && as.logical(column_reqs["sql", col]) && is_delimited(population_table[[row, col]], "[]")
      txt_check <- not_na_check && as.logical(column_reqs["txt", col]) && is_delimited(population_table[[row, col]], "\"")

      if (!not_na_check) {
        any_check_failed <- TRUE
        warning(sprintf("missing input in population control: column %s, row %d", col, row))
      } else if (!sql_check & !txt_check) {
        any_check_failed <- TRUE
        warning(sprintf(
          "unaccepted input in population control: column %s, row %d - acceptable values are %s",
          col, row, column_reqs["accepts", col]
        ))
      }

      any_check_failed <- any_check_failed || no_internal_delimiters(population_table[[row, col]], "population control")
    }
  }
  run_time_inform_user("population and period table checked", context = "details", print_level = control_verbose)

  #### measures and indicators (1 of 2) ----

  # acceptable and default values
  column_reqs <- data.frame(
    Database_name              = c("sql only", TRUE, FALSE),
    Schema_name                = c("sql only", TRUE, FALSE),
    Table_name                 = c("sql only", TRUE, FALSE),
    Identity_column            = c("sql or txt", TRUE, TRUE),
    Measure_period_start_date  = c("sql or txt", TRUE, TRUE),
    Measure_period_end_date    = c("sql or txt", TRUE, TRUE),
    Label_measure              = c("sql or txt", TRUE, TRUE),
    Value_measure              = c("sql or txt", TRUE, TRUE),
    stringsAsFactors = FALSE,
    row.names = c("accepts", "sql", "txt")
  )

  # required columns
  colnames(column_reqs) <- tolower(colnames(column_reqs))
  for (col in colnames(column_reqs)) {
    assert(col %in% colnames(measure_table), sprintf("column %s missing from measure input table", col))
  }

  # required values
  for (col in colnames(column_reqs)) {
    for (row in 1:nrow(measure_table)) {
      # not missing
      not_na_check <- !is.na(measure_table[[row, col]])
      # required delimiters
      sql_check <- not_na_check && as.logical(column_reqs["sql", col]) && is_delimited(measure_table[[row, col]], "[]")
      txt_check <- not_na_check && as.logical(column_reqs["txt", col]) && is_delimited(measure_table[[row, col]], "\"")

      if (!not_na_check) {
        any_check_failed <- TRUE
        warning(sprintf("missing input in measure control: column %s, row %d", col, row))
      } else if (!sql_check & !txt_check) {
        any_check_failed <- TRUE
        warning(sprintf(
          "unaccepted input in measure control: column %s, row %d - acceptable values are %s",
          col, row, column_reqs["accepts", col]
        ))
      }

      any_check_failed <- any_check_failed || no_internal_delimiters(measure_table[[row, col]], "measure control")
    }
  }
  run_time_inform_user("measures and indicators (1 of 2) checked", context = "details", print_level = control_verbose)

  #### measures and indicators (2 of 2) ----

  # acceptable and default values
  column_reqs <- list(
    Measure_summarised_by = c("SUM", "COUNT", "EXISTS", "MIN", "MAX", "DURATION", "HISTOGRAM", "DISTINCT", "MEAN"),
    Proportional = c("true", "t", "false", "f")
  )

  # required columns
  names(column_reqs) <- tolower(names(column_reqs))
  for (col in names(column_reqs)) {
    assert(col %in% colnames(measure_table), sprintf("column %s missing from measure input table", col))
  }

  # required values
  for (col in names(column_reqs)) {
    for (row in 1:nrow(measure_table)) {
      not_na_check <- !is.na(measure_table[[row, col]])
      check <- not_na_check && tolower(measure_table[[row, col]]) %in% tolower(column_reqs[[col]])

      if (!not_na_check) {
        any_check_failed <- TRUE
        warning(sprintf("missing input in measure control: column %s, row %d", col, row))
      } else if (!check) {
        any_check_failed <- TRUE
        warning(sprintf(
          "unaccepted input in measure control: column %s, row %d - acceptable values are %s",
          col, row, column_reqs[[col]]
        ))
      }
    }
  }
  run_time_inform_user("measures and indicators (2 of 2) checked", context = "details", print_level = control_verbose)

  #### close ----
  msg <- sprintf(
    "%s\n%s\n%s\n%s",
    "at least one input table contains unaccepted inputs",
    "sql inputs must be delimited with [ ], e.g. [snz_uid]",
    "txt inputs must be delimited with \", e.g. \"label\"",
    "delimiters may not appear within inputs, e.g. [not]acceptable]"
  )
  assert(!any_check_failed, msg)
}

## validate database tables -----------------------------------------------------------------------
#'
#' Check that all tables requested in input tables exist
#' and that their required columns exist
#'
validate_database_tables <- function(input_table, control_verbose, output_database) {
  #### setup ----
  # connect to db
  db_con <- create_database_connection(database = output_database)
  # key cols
  col_names <- colnames(input_table)
  remove <- c("database_name", "schema_name", "table_name")
  col_names <- col_names[col_names %not_in% remove]

  #### checks for each row ----
  for (ii in 1:nrow(input_table)) {
    # this iteration
    this_db <- input_table[[ii, "database_name"]]
    this_schema <- input_table[[ii, "schema_name"]]
    this_table <- input_table[[ii, "table_name"]]

    # check table exists while connecting to table
    tmp_table <- create_access_point(db_con, this_db, this_schema, this_table)

    # iterate through non-database columns
    for (col in col_names) {
      this_control_value <- input_table[[ii, col]]
      tbl_col <- remove_delimiters(this_control_value, "[]")

      # check column in table
      if (is_delimited(this_control_value, "[]")) {
        assert(tbl_col %in% colnames(tmp_table), sprintf("column %s missing from table %s", this_control_value, this_table))
      }

      # check value column data type
      if ("measure_summarised_by" %in% col_names & col == "value_measure" & is_delimited(this_control_value, "[]")) {
        summary_type <- input_table[[ii, "measure_summarised_by"]]

        if (summary_type %in% c("SUM", "MIN", "MAX")) {
          var_example <- tmp_table %>%
            select(!!sym(tbl_col)) %>%
            filter(!is.na(!!sym(tbl_col))) %>%
            head(1) %>%
            collect() %>%
            unlist(use.names = FALSE)
          assert(
            is.numeric(var_example) | is.logical(var_example),
            sprintf("You cannot %s column %s of table %s because it is not numeric", summary_type, this_control_value, this_table)
          )
        }
      }
    }
  }

  #### close ----
  close_database_connection(db_con)
  run_time_inform_user("database table validated", context = "details", print_level = control_verbose)
}

## create output table ----------------------------------------------------------------------------
#'
#' Create the new table and populate it. Records are appended
#' to the new table for every combination of population and
#' measure.
#'
assemble_output_table <- function(population_table, measure_table,
                                  output_database, output_schema, output_table,
                                  control_development_mode, control_overwrite_output_table, control_verbose) {
  #### existence of output table ----
  # connect to db
  db_con <- create_database_connection(database = output_database)

  # delete
  if (control_overwrite_output_table) {
    delete_table(db_con, output_database, output_schema, output_table)
  }
  # required table
  output_columns <- list(
    identity_column = "[int] NOT NULL",
    label_identity = "[varchar](50) NOT NULL",
    summary_period_start_date = "[date] NOT NULL",
    summary_period_end_date = "[date] NOT NULL",
    label_summary_period = "[varchar](50) NOT NULL",
    label_measure = "[varchar](70) NOT NULL",
    value_measure = "[FLOAT](53) NULL"
  )
  # create if does not exist
  if (!table_or_view_exists_in_db(db_con, output_database, output_schema, output_table)) {
    run_time_inform_user("creating table", context = "all", print_level = control_verbose)
    create_table(db_con, output_database, output_schema, output_table, output_columns, OVERWRITE = FALSE)
  }
  # confirm table has required columns
  out_tbl <- create_access_point(db_con, output_database, output_schema, output_table)
  assert(table_contains_required_columns(out_tbl, names(output_columns), only = TRUE), "output table missing column")
  close_database_connection(db_con)
  run_time_inform_user("existence of output table verified", context = "details", print_level = control_verbose)

  #### access and append values ----

  # for each row in population table
  for (row_p in 1:nrow(population_table)) {
    # values
    p_identity_column <- prep_for_sql(population_table[[row_p, "identity_column"]], alias = "p")
    p_identity_label <- prep_for_sql(population_table[[row_p, "label_identity"]], alias = "p")
    p_start_date <- prep_for_sql(population_table[[row_p, "summary_period_start_date"]], alias = "p")
    p_end_date <- prep_for_sql(population_table[[row_p, "summary_period_end_date"]], alias = "p")
    p_period_label <- prep_for_sql(population_table[[row_p, "label_summary_period"]], alias = "p")

    # for each row in measure table
    for (row_m in 1:nrow(measure_table)) {
      # values
      m_identity_column <- prep_for_sql(measure_table[[row_m, "identity_column"]], alias = "m")
      m_start_date <- prep_for_sql(measure_table[[row_m, "measure_period_start_date"]], alias = "m")
      m_end_date <- prep_for_sql(measure_table[[row_m, "measure_period_end_date"]], alias = "m")
      m_label <- prep_for_sql(measure_table[[row_m, "label_measure"]], alias = "m")
      m_value <- prep_for_sql(measure_table[[row_m, "value_measure"]], alias = "m")

      # connect
      db_con <- create_database_connection(database = output_database)

      # components
      from_population <- sprintf(
        "%s.%s.%s",
        population_table[[row_p, "database_name"]],
        population_table[[row_p, "schema_name"]],
        population_table[[row_p, "table_name"]]
      )
      from_measure <- sprintf(
        "%s.%s.%s",
        measure_table[[row_m, "database_name"]],
        measure_table[[row_m, "schema_name"]],
        measure_table[[row_m, "table_name"]]
      )
      optional_top <- ifelse(control_development_mode, " TOP 1000 ", " ")

      calculation <- handle_summary_case(
        summary_type = measure_table[[row_m, "measure_summarised_by"]],
        proportional = as.logical(measure_table[[row_m, "proportional"]]),
        m_label, m_value,
        m_start_date, m_end_date, p_start_date, p_end_date
      )

      group_by_columns <- c(p_identity_column, p_identity_label, p_start_date, p_end_date, p_period_label, calculation$group)
      group_by_columns <- group_by_columns[!is_delimited(group_by_columns, "'")]
      GROUP_BY <- ifelse(length(group_by_columns) == 0, "", paste0("GROUP BY ", paste0(group_by_columns, collapse = ", ")))

      # prepare query
      sql_query <- dbplyr::build_sql(
        con = db_con,
        sql(glue::glue(
          "SELECT {optional_top}\n",
          "       {p_identity_column} AS [identity_column]\n",
          "      ,{p_identity_label}  AS [label_identity]\n",
          "      ,{p_start_date} AS [summary_period_start_date]\n",
          "      ,{p_end_date}   AS [summary_period_end_date]\n",
          "      ,{p_period_label}  AS [label_summary_period]\n",
          "      ,{calculation$label} AS [label_measure]\n",
          "      ,{calculation$value} AS [value_measure]\n",
          "FROM {from_population} AS p\n",
          "INNER JOIN {from_measure} AS m\n",
          "ON {p_identity_column} = {m_identity_column}\n",
          "AND {p_start_date} <= {m_end_date}\n",
          "AND {m_start_date} <= {p_end_date}\n",
          "WHERE {calculation$label} IS NOT NULL\n",
          "{GROUP_BY}"
        ))
      )
      table_to_append <- dplyr::tbl(db_con, dbplyr::sql(sql_query))

      # append & conclude
      append_database_table(db_con, output_database, output_schema, output_table,
        list_of_columns = names(output_columns), table_to_append
      )
      close_database_connection(db_con)
      run_time_inform_user(sprintf(
        "completed population %3d of %3d, measure %4d of %4d",
        row_p, nrow(population_table), row_m, nrow(measure_table)
      ),
      context = "details", print_level = control_verbose
      )
    }
  }

  #### tidy up ----
  #
  # compress and index not done on long-thin table as can interfere with appending.
}

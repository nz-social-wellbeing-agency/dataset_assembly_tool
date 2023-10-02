###############################################################################
#' Description: dbplyr support functions for R development with SQL server
#'
#' Input: Connection details that must be manually set.
#'
#' Output: support functions for ease of using R to manipulate SQL tables.
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: odbc, DBI, dplyr, dbplyr packages, utility_functions.R
#'
#' Notes:
#' - Connection details (line ~35) NOT FOR RELEASE!!
#' - Some functions (and tests) assume SQL Server environment
#'   due to differences in syntax between different implementations of SQL.
#' - Uses code folding by headers (Alt + O to collapse all)
#' - Example use included at end of file.
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-11-18 SA v2 for release
#' 2020-07-12 SA addition of collapse indicator column function
#' 2020-02-13 SA addition of pivot function
#' 2020-01-09 SA v1 split dbplyr support out from utility_functions.R
#' #############################################################################

# connection details
DEFAULT_SERVER <- "[]"
DEFAULT_DATABASE <- "[]"
DEFAULT_PORT <- NA
# DO NOT RELEASE THESE VALUES
# DO NOT RELEASE THESE VALUES

# error if connection details are missing
if (is.na(DEFAULT_SERVER) | nchar(DEFAULT_SERVER) < 4 |
    is.na(DEFAULT_DATABASE) | nchar(DEFAULT_DATABASE) < 4) {
  stop("Default server and database must be set in dbplyr_helper_functions.R")
}

# library
library(DBI)
library(dplyr)
library(dbplyr)

## Create connection string -----------------------------------------------------------------------
#'
#' ODBC connections require a connection string to define the connection
#' between the database and the access point. This string has a default
#' format that this function satisfies.
#'
#' This function does not need to be called directly to use the utility
#' functions. It is called by create_database_connection.
#'
set_connection_string <- function(server, database, port = NA) {
  connstr <- "DRIVER=ODBC Driver 18 for SQL Server; "
  connstr <- paste0(connstr, "Trusted_Connection=Yes; ")
  connstr <- paste0(connstr, "TrustServerCertificate=Yes; ")
  connstr <- paste0(connstr, "DATABASE=", database, "; ")
  connstr <- paste0(connstr, "SERVER=", server)
  if (!is.na(port)) {
    connstr <- paste0(connstr, ", ", port)
  }
  return(connstr)
}

## Create database connection point ---------------------------------------------------------------
#'
#' Any arguments passed to the function need to be named.
#' Default values are set at the top of this script.
#'
#' A single connection can access multiple associated databases.
#' We recommend using a single connection to access all tables
#' because attempts to join tables from different connections
#' (even if they are in the same database) perform poorly if
#' at all.
#'
create_database_connection <- function(..., server = NA, database = NA, port = NA) {
  # checks
  assert("odbc" %in% installed.packages(), "odbc package must be installed to connect to database")
  assert(length(list(...)) == 0, "all database connection arguments must be named")

  # default values
  if (is.na(server)) {
    server <- DEFAULT_SERVER
  }
  if (is.na(database)) {
    database <- DEFAULT_DATABASE
  }
  if (is.na(port)) {
    port <- DEFAULT_PORT
  }

  server <- remove_delimiters(server, "[]")
  database <- remove_delimiters(database, "[]")

  # connect
  connection_string <- set_connection_string(server, database, port)
  db_connection <- DBI::dbConnect(odbc::odbc(), .connection_string = connection_string)
}

## Close an open database connection --------------------------------------------------------------
#'
#' Good practice is to close open connections once they are finished with.
#' Large numbers of open connections can degrade performance.
#'
close_database_connection <- function(db_connection) {
  DBI::dbDisconnect(db_connection)
}

## Append database name to schema name ------------------------------------------------------------
#'
db_schema <- function(db, schema) {
  assert(is_delimited(db, "[]"), "database not correctly delimited")
  assert(is_delimited(schema, "[]"), "schema not correctly delimited")

  return(glue::glue("{db}.{schema}"))
}

## Delimited checks -------------------------------------------------------------------------------
#'
#' For consistency of approach it is recommended that SQL db, scema and table names
#' be delimited using square brackets: "[]".
#'
#' This function provides a standardised warning for missing delimiters that can be
#' inserted at the start of every function.
#'
#' Also checks for special characters in all three
#'
warn_if_missing_delimiters <- function(db, schema, tbl_name) {
  if (!is_delimited(db, "[]")) {
    warning("db is not delimited, delimiting with [] is recommended")
  }
  if (!is_delimited(schema, "[]")) {
    warning("schema is not delimited, delimiting with [] is recommended")
  }
  if (!is_delimited(tbl_name, "[]")) {
    warning("tbl_name is not delimited, delimiting with [] is recommended")
  }

  no_special_characters(db)
  no_special_characters(schema)
  no_special_characters(tbl_name)
}

## Map SQL table for access in R ------------------------------------------------------------------
#' Creates access point for R to run queries on SQL server.
#'
#' Use this in place of loading a table into R. It will be treated as an R dataframe
#' but the data will remain on the SQL server (instead of in R memory).
#'
#' The same db_connection must be used foreach table access point. Otherwise
#' you will not be able to join tables together. Tables from different databases
#' within the same server can be accessed bythe same connection.
#'
#' If table does not exist you will receive an error.
#' If you do not have permission to access to all the table columns you will receive an error.
#' Recommended solution: Create a View that only selects columns you have permission for.
#'
create_access_point <- function(db_connection, db, schema, tbl_name) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )

  tryCatch(
    {
      # access table
      if (nchar(schema) > 0 | nchar(db) > 0) {
        table_access <- dplyr::tbl(
	  db_connection,
	  from = dbplyr::in_schema(
            dbplyr::sql(db_schema(db, schema)),
            dbplyr::sql(tbl_name)
          )
	)
      } else {
        table_access <- dplyr::tbl(db_connection, from = tbl_name)
      }
    },
    error = function(cond) {
      message(
        "Error accessing SQL table, you might lack permission for all columns of this table.\n",
        "Recommended solution: Create a View of the columns you have permission for and access this."
      )
      message("Here is the original error message:")
      message(cond)
      return(NA)
    }
  )
  return(table_access)
}

## write out interum table for reuse --------------------------------------------------------------
#'
#' Takes an R object (tbl_to_save) that is an SQL table that has been
#' manipulated/transformed and saves it as a new table in SQL.
#' Returns a connection to the new table.
#'
#' For complex or extended analyses, this is recommended as it reduces
#' the complexity of the underlying SQL query that defines the manipulated
#' table.
#'
write_for_reuse <- function(db_connection, db, schema, tbl_name, tbl_to_save, index_columns = NA) {
  # check input
  warn_if_missing_delimiters(db, schema, tbl_name)

  run_time_inform_user("writing temporary table", context = "all")
  saved_table <- write_to_database(tbl_to_save, db_connection, db, schema, tbl_name, OVERWRITE = TRUE)
  run_time_inform_user("completed write", context = "all")

  if (length(index_columns) > 1 | !is.na(index_columns[1])) {
    result <- create_nonclustered_index(db_connection, db, schema, tbl_name, index_columns)
    run_time_inform_user("added index", context = "all")
  }

  return(saved_table)
}

## Check table or view exists in database ---------------------------------------------------------
#'
#' Returns true is table or view exists in the database
#' Used in place of 'dbExistsTable' and 'tbl %in% DBI::dbListTables(db_con)'
#' Because these approaches do not always handle schema consistently.
#'
table_or_view_exists_in_db <- function(db_connection, db, schema, tbl_name) {
  # check input
  warn_if_missing_delimiters(db, schema, tbl_name)

  query <- glue::glue(
    "IF OBJECT_ID('{db}.{schema}.{tbl_name}', 'U') IS NOT NULL\n",
    "OR OBJECT_ID('{db}.{schema}.{tbl_name}', 'V') IS NOT NULL\n",
    "SELECT 1 AS ans ELSE SELECT 0 AS ans"
  )

  exists <- DBI::dbGetQuery(db_connection, query)
  return(unlist(exists, use.names = FALSE) == 1)
}

## Check table has required columns ---------------------------------------------------------------
#'
#' Returns TRUE if the table contains all the required columns.
#' If only = TRUE, returns TRUE if the table ONLY contains all
#' the required columns.
#'
#' Checks column names only, does not consider contents.
#'
table_contains_required_columns <- function(tbl_to_check, required_columns, only = FALSE) {

  # column names of table
  table_column_names <- colnames(tbl_to_check)

  # required columns in table
  correct <- all(required_columns %in% table_column_names)
  # only required columns in table
  if (only) {
    correct <- correct & all(table_column_names %in% required_columns)
  }

  return(correct)
}

## Union all --------------------------------------------------------------------------------------
#'
#'  Provides UNION ALL functionality from SQL. Appends two tables.
#'
#' Requires a input tables to have identical column names. Provides as output
#' a single table the "union all" of all the input tables.
#'
union_all <- function(table_a, table_b, list_of_columns) {
  assert("tbl_sql" %in% class(table_a), "input table is not a remote table")
  assert("tbl_sql" %in% class(table_b), "input table is not a remote table")
  # connection
  db_connection <- table_a$src$con

  table_a <- table_a %>% dplyr::ungroup() %>% dplyr::select(all_of(list_of_columns))
  table_b <- table_b %>% dplyr::ungroup() %>% dplyr::select(all_of(list_of_columns))

  sql_query <- dbplyr::build_sql(
    con = db_connection,
    dbplyr::sql_render(table_a),
    "\nUNION ALL\n",
    dbplyr::sql_render(table_b)
  )
  return(dplyr::tbl(db_connection, dbplyr::sql(sql_query)))
}

## Delete (drop) tables and views from SQL. -------------------------------------------------------
#'
#' Checks for existence of table/view prior to dropping.
#' Can only drop views if the input connection is to the database containing
#' the views.
#'
delete_table <- function(db_connection, db, schema, tbl_name, mode = "table") {
  warn_if_missing_delimiters(db, schema, tbl_name)
  mode <- tolower(mode)
  assert(mode %in% c("view", "table"), "mode must be in (view, table)")

  # type
  if (mode == "table") code <- "U"
  if (mode == "view") code <- "V"

  # remove database name if view mode
  maybe_db_schema <- db_schema(db, schema)
  if (mode == "view") {
    maybe_db_schema <- schema
  }

  # remove table if it exists
  removal_query <- glue::glue(
    "IF OBJECT_ID('{maybe_db_schema}.{tbl_name}', '{code}') IS NOT NULL\n",
    "DROP {toupper(mode)} {maybe_db_schema}.{tbl_name};"
  )
  save_to_sql(removal_query, paste0("delete_", mode))
  result <- DBI::dbExecute(db_connection, as.character(removal_query))
}

## Write to database ------------------------------------------------------------------------------
#' Returning connection to the new table
#'
#' Given a table from a database connection, writes to a new table using the
#' SELECT ... INTO ... FROM ... pattern.
#'
#' Original table must come from an SQL connection.
#' Does not write R tables into SQL. Use copy_r_to_sql for this.
#'
#' E.g. the following works
#'       my_table <- create_access_point(....)
#'       write_to_database(my_table, ...)
#' But this will not work
#'       my_table <- data.frame(x = 1:10, y = 1:10)
#'       write_to_database(my_table, ...)
#'
write_to_database <- function(input_tbl, db_connection, db, schema, tbl_name, OVERWRITE = FALSE) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  # checks
  assert("tbl_sql" %in% class(input_tbl), "input table must originate from sql connection")

  # remove table if it exists
  if (OVERWRITE) delete_table(db_connection, db, schema, tbl_name)

  # connection
  tbl_connection <- input_tbl$src$con
  # setup
  from_id <- dbplyr::ident(paste0("long", floor(runif(1, 1000000, 9999999))))

  # SQL query
  sql_query <- glue::glue(
    "SELECT *\n",
    "INTO {db}.{schema}.{tbl_name}\n",
    "FROM (\n",
    dbplyr::sql_render(input_tbl),
    "\n) {from_id}"
  )

  # run query
  save_to_sql(sql_query, "write_to_database")
  result <- dbExecute(db_connection, as.character(sql_query))

  # load and return new table
  create_access_point(db_connection, db, schema, tbl_name)
}

## Add nonclustered index to a table -----------------------------------------------------------------
#'
#' Create a nonclustered index to improve table performance.
#' Unlike clustered indexes, multiple nonclustered indexes can be created.
#' 
#' Permanent high reuse tables may benefit from clustered indexes. But most researcher-created
#' tables are used a limited number of times and hence do not justify the additional overhead
#' of creating a clustered index. Non-clustered indexes are recommended in these cases.
#'
create_nonclustered_index <- function(db_connection, db, schema, tbl_name, index_columns) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  # table in connection
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )
  # columns are in table
  assert(
    all(index_columns %in% colnames(create_access_point(db_connection, db, schema, tbl_name))),
    "database table does not have the required columns"
  )

  index_columns <- sapply(index_columns, add_delimiters, delimiter = "[]")
  index_columns <- paste0(index_columns, collapse = ", ")

  query <- glue::glue("CREATE NONCLUSTERED INDEX my_index_name ON {db}.{schema}.{tbl_name} ({index_columns})")

  # print(query)
  save_to_sql(query, "add_nonclustered_index")
  result <- DBI::dbExecute(db_connection, as.character(query))
}

## Add clustered index to a table -----------------------------------------------------------------
#'
#' Note: at most a single clustered index can be added to each table.
#' This operation is potentially expensive, so should be used only where needed.
#' 
#' For researcher created tables non-clustered indexes are recommended.
#' This function only provided for backwards compatibility.
#'
create_clustered_index <- function(db_connection, db, schema, tbl_name, index_columns) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  # table in connection
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )
  # columns are in table
  assert(
    all(index_columns %in% colnames(create_access_point(db_connection, db, schema, tbl_name))),
    "database table does not have the required columns"
  )
  
  index_columns <- sapply(index_columns, add_delimiters, delimiter = "[]")
  index_columns <- paste0(index_columns, collapse = ", ")
  
  query <- glue::glue("CREATE CLUSTERED INDEX my_index_name ON {db}.{schema}.{tbl_name} ({index_columns})")
  
  # print(query)
  save_to_sql(query, "add_clustered_index")
  result <- DBI::dbExecute(db_connection, as.character(query))
}

## Append rows to an existing table ---------------------------------------------------------------
#'
#' Given a table from a database connection, append it to an existing table
#' using the INSERT INTO ... (COLUMN NAMES) SELECT ... FROM ... pattern.
#'
#' Like write_to_database, the original table must come from an SQL connection.
#' Does not append R tables onto SQL tables. You first need to write the R table
#' into SQL.
#'
#' A common error occurs when character strings in the appended table exceed the
#' length of the existing varchar(n) limit in the original dataset.
#' E.g. appending the string 'ABCDE' into a varchar(3) column will error.
#'
append_database_table <- function(db_connection, db, schema, tbl_name, list_of_columns, table_to_append) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  assert("tbl_sql" %in% class(table_to_append), "table to append must originate from sql connection")
  # table in connection
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )
  # tables contain list of columns
  assert(
    all(list_of_columns %in% colnames(table_to_append)),
    "table to append does not have required columns"
  )
  assert(
    all(list_of_columns %in% colnames(create_access_point(db_connection, db, schema, tbl_name))),
    "database table does not have the required columns"
  )

  table_to_append <- table_to_append %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(list_of_columns))

  sql_list_of_columns <- paste0(dbplyr::escape(dbplyr::ident(list_of_columns), con = db_connection), collapse = ", ")

  query <- glue::glue("INSERT INTO {db}.{schema}.{tbl_name} ({sql_list_of_columns})\n {dbplyr::sql_render(table_to_append)}")

  # print(query)
  save_to_sql(query, "append_table")
  result <- DBI::dbExecute(db_connection, as.character(query))
}

## Create new table in the database ---------------------------------------------------------------
#'
#' Creates an empty table in the database, overwriting an existing copy if
#' instructed. Main use is to setup an empyt table that results can be iteratively
#' appended to.
#'
#' Not necessary if creating a table directly from an existing table.
#' Use write_to_database for this.
#'
#' named_list_of_columns takes the format name = "sql type"
#' For example:
#' list_of_columns <- (number = "[int] NOT NULL",
#'                     date = "[date] NOT NULL",
#'                     character = "[varchar](25) NULL")
#'
create_table <- function(db_connection, db, schema, tbl_name, named_list_of_columns, OVERWRITE = FALSE) {
  warn_if_missing_delimiters(db, schema, tbl_name)

  # remove table if it exists
  if (OVERWRITE) delete_table(db_connection, db, schema, tbl_name)

  # setup queries
  DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_NULLS ON")))
  DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET QUOTED_IDENTIFIER ON")))
  DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_PADDING ON")))

  # main SQL query
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "CREATE TABLE ",
    dbplyr::sql(db), ".",
    dbplyr::sql(schema), ".",
    dbplyr::sql(tbl_name), "(", "\n",
    dbplyr::sql(paste0("[", names(named_list_of_columns), "] ",
                       named_list_of_columns, collapse = ",\n")), "\n",
    ") ON [PRIMARY]"
  )

  # run query
  save_to_sql(sql_query, "create_table")
  result <- DBI::dbExecute(db_connection, as.character(sql_query))
  # post queries
  DBI::dbExecute(db_connection, as.character(dbplyr::build_sql(con = db_connection, "SET ANSI_PADDING OFF")))
  return(result)
}

## Save SQL queries to files ----------------------------------------------------------------------
#'
#' All the SQL queries that write or change data on the server (but not those that
#' only fetch data) in these utility functions save an SQL code file of the command
#' that was executed.
#'
#' These scripts are primarily intended to support debugging.
#' They can be deleted without concern.
#'
save_to_sql <- function(query, desc) {
  if (!dir.exists("./SQL tmp scripts")) {
    dir.create("./SQL tmp scripts")
  }

  Sys.sleep(0.1) # tiny delay ensures no two files writes can have the same time-stamp
  clean_time <- gsub("[.:]", "-", format(Sys.time(), "%Y-%m-%d %H%M%OS3")) # includes milliseconds now
  clean_name <- gsub("[. :]", "_", desc)

  file_name <- paste0("./SQL tmp scripts/", clean_time, " ", clean_name, ".sql")

  writeLines(as.character(query), file_name)
}

## Create views -----------------------------------------------------------------------------------
#' Returning connection to the new view
#'
#' The view equivalent of write_to_database. Given a table from a database
#' connection, defines a new view with this definition.
#'
#' The original table must come from an SQL connection.
#' Can only create views if the input connection is to the database containing
#' the views.
#'
create_view <- function(tbl_name, db_connection, db, schema, view_name, OVERWRITE = FALSE) {
  warn_if_missing_delimiters(db, schema, view_name)
  # checks
  assert("tbl_sql" %in% class(tbl_name), "input table must originate from sql connection")

  # remove view if it exists
  if (OVERWRITE) delete_table(db_connection, db, schema, view_name, mode = "view")

  # SQL query
  sql_query <- glue::glue(
    "CREATE VIEW {schema}.{view_name} AS\n",
    "{dbplyr::sql_render(tbl_name)}\n"
  )

  # run query
  save_to_sql(sql_query, "create_view")
  result <- DBI::dbExecute(db_connection, as.character(sql_query))

  # load and return new table
  create_access_point(db_connection, db, schema, view_name)
}

## Copy R table to SQL ----------------------------------------------------------------------------
#'
#' The inbuilt dbplyr copy_to function does not appear to work in our set-up.
#' Worse, it caused errors/locking, preventing other users who are using the
#' same connection method until their R session is restarted.
#'
#' Hence we implement a established work around using the DBI package.
#' This is a revision of the original function (in previous versions),
#' and uses different functionality to simplify the process.
#'
copy_r_to_sql <- function(db_connection, db, schema, sql_table_name, r_table_name, OVERWRITE = FALSE) {
  warn_if_missing_delimiters(db, schema, sql_table_name)

  # remove if overwrite
  if (OVERWRITE) delete_table(db_connection, db, schema, sql_table_name)

  suppressMessages( # mutes translation message
    DBI::dbWriteTable(
      db_connection,
      DBI::Id(
        catalog = remove_delimiters(db, "[]"),
        schema = remove_delimiters(schema, "[]"),
        table = remove_delimiters(sql_table_name, "[]")
      ),
      r_table_name
    )
  )

  r_table_name <- create_access_point(db_connection, db, schema, sql_table_name)
}

## Compress a table -------------------------------------------------------------------------------
#'
#' Large SQL tables can be compressed to reduce the space they take up.
#'
compress_table <- function(db_connection, db, schema, tbl_name) {
  warn_if_missing_delimiters(db, schema, tbl_name)
  assert(
    table_or_view_exists_in_db(db_connection, db, schema, tbl_name),
    glue::glue("{db}.{schema}.{tbl_name} not found in database")
  )

  # SQL query
  sql_query <- glue::glue("ALTER TABLE {db}.{schema}.{tbl_name} REBUILD PARTITION = ALL WITH (DATA_COMPRESSION = PAGE)")

  # run query
  save_to_sql(sql_query, "compress_table")
  result <- DBI::dbExecute(db_connection, as.character(sql_query))
}

## Pivot long-thin table to rectangular table -----------------------------------------------------
#'
#' Takes label-value pair of columns and transforms table
#' so labels that were column entries become column names.
#'
#' SQL equivalent of tidyr::spread
#'
#' Note that this tends to be an expensive operation.
#' So best not to include it as part of a long or complex
#' chain of commands.
#'
pivot_table <- function(input_tbl, label_column, value_column, aggregator = "SUM") {
  # checks
  assert("tbl_sql" %in% class(input_tbl), "input table must originate from sql connection")
  assert(label_column %in% colnames(input_tbl), sprintf("label column [%s] is not in table", label_column))
  assert(value_column %in% colnames(input_tbl), sprintf("value column [%s] is not in table", value_column))
  no_special_characters(aggregator)

  # connection
  db_connection <- input_tbl$src$con

  # pivot components
  non_pivot_columns <- colnames(input_tbl)
  non_pivot_columns <- non_pivot_columns[non_pivot_columns %not_in% c(label_column, value_column)]

  pivot_columns <- input_tbl %>%
    dplyr::select(!!sym(label_column)) %>%
	dplyr::filter(!is.na(!!sym(label_column))) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    unlist(use.names = FALSE) %>%
    sort()

  # check no special characters in new column labels
  # sapply(pivot_columns, no_special_characters)

  # query components
  from_id <- paste0("long", floor(runif(1, 1000000, 9999999)))
  pivot_id <- paste0("pvt", floor(runif(1, 1000000, 9999999)))
  non_pivot_columns <- paste0("[", non_pivot_columns, "]", collapse = ", ")
  pivot_columns <- paste0("[", pivot_columns, "]", collapse = ", ")
  value_column <- add_delimiters(value_column, "[]")

  # build SQL pivot query
  sql_query <- glue::glue(
    "SELECT {non_pivot_columns}, \n{pivot_columns}\n",
    "FROM (\n",
    "{dbplyr::sql_render(input_tbl)}\n",
    ") {from_id}\n",
    "PIVOT (\n",
    "{aggregator}({value_column}) FOR {label_column} IN \n({pivot_columns})\n",
    ") AS {pivot_id}"
  )

  #   dbplyr::build_sql(
  #   con = db_connection
  #   ,"SELECT ", dbplyr::sql(non_pivot_columns)
  #   ,"\n, ", dbplyr::sql(pivot_columns)
  #   ,"\nFROM (\n"
  #   ,dbplyr::sql_render(input_tbl)
  #   ,"\n) ", dbplyr::ident(from_id), "\n"
  #   ,"PIVOT (\n"
  #   ,dbplyr::sql(aggregator),"(", dbplyr::escape(dbplyr::ident(value_column), con = db_connection), ")"
  #   ,"\nFOR ", dbplyr::escape(dbplyr::ident(label_column), con = db_connection)
  #   , "\nIN (", dbplyr::sql(pivot_columns), ")"
  #   ,"\n) AS ", dbplyr::ident(pivot_id)
  # )

  return(dplyr::tbl(db_connection, dbplyr::sql(sql_query)))
}

## collapse indicator columns ---------------------------------------------------------------------
#'
#' Many tables have mutually exclusive indicator columns.
#' This function provides a compact way to combine these columns to a single column
#' where the original columns all share the same prefix.
#'
#' For example: sex_male = c("y", "n", "n"), sex_female = c("n", "y", "y")
#' Becomes: sex = c("male", "female", "female")
#'
#' Where columns are not mutually exclusive, this function should not be used as
#' only a single value will be preserved in the output column and any duplication
#' will be removed invisably.
#'
#' For example: eye_blue = c(1,1,0), eye_brown = c(1,1,1), eye_green = c(0,1,1)
#' Becomes eye = c("blue","blue","brown")
#'
collapse_indicator_columns <- function(input_tbl, prefix, yes_values, label = NA) {
  # validate input
  assert(is.data.frame(input_tbl) | is.tbl(input_tbl), "input table must be a dataframe")
  assert(is.character(prefix), "prefix must be a character string")
  assert(length(yes_values) >= 1, "at least one yes value must be provided")
  if (is.na(label)) {
    label <- prefix
  }
  assert(is.character(label), "label must be a character string")

  # column names
  colnames_list <- colnames(input_tbl)
  matches <- grepl(paste0("^", prefix), colnames_list)
  colnames_match <- colnames_list[matches]
  colnames_nonmatch <- colnames_list[!matches]

  colnames_match <- sort(colnames_match)

  suffix_list <- sub(prefix, "", colnames_match)

  # warn if no matches
  if (length(colnames_match) == 0) {
    warning("prefix matches no column names")
    return(input_tbl)
  }

  yes_text <- paste0("c(", paste0(yes_values, collapse = ","), ")")

  # collapse
  output_tbl <- input_tbl %>%
    mutate(!!sym(label) := case_when(
      !!!rlang::parse_exprs(paste0("`", colnames_match, "` %in% ", yes_text, " ~ '", suffix_list, "'"))
    )) %>%
    dplyr::select(all_of(c(colnames_nonmatch, label)))
}

## EXAMPLE USAGE ##############################################################
# 2020-03-18
#
# # source
# > source('utility_functions.R')
# > source('dbplyr_helper_functions.R")
#
# # setup
# > our_database = "[our_database]"
# > our_schema = "[our_schema]"
# > db_con = create_database_connection(database = our_database)
#
# # connect
# > my_table = create_access_point(db_connection =  db_con, db = our_database, schema =  our_schema, tbl_name = "example_table")
#
# # use
# > my_table = my_table %>%
#     filter(name == 'James Bond') %>%
#     mutate(code = '007') %>%
#     select(name, code)
#
# # check underlying SQL code
# > my_table %>% show_query()
#
# # fetch data into R
# > my_table = my_table %>% collect()
#
# # close connection
# > close_database_connection(db_con)
#' #############################################################################

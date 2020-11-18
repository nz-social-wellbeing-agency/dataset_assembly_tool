###############################################################################
#' Description: Helper functions for data assembly tool
#'
#' Input:
#'
#' Output:
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: utility_functions.R
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

## Read csv, xls or xlsx file input ---------------------------------------------------------------
#' Autodetecting which file read command is needed
#'
read_table_file <- function(file_name_and_path) {
  extension <- tools::file_ext(file_name_and_path)

  if (extension == "xls") {
    file_contents <- readxl::read_xls(file_name_and_path)
  } else if (extension == "xlsx") {
    file_contents <- readxl::read_xlsx(file_name_and_path)
  } else if (extension == "csv") {
    file_contents <- read.csv(file_name_and_path, stringsAsFactors = FALSE)
  } else {
    stop(sprintf("unrecognised file extension: %s", extension))
  }
  return(as.data.frame(file_contents, stringsAsFactors = FALSE))
}

## No internal delimiters -------------------------------------------------------------------------
#'
#' Provides a warning or error if internal delimiters exist
#'
no_internal_delimiters <- function(string, table_label) {
  internal_delim <- FALSE

  if (is_delimited(string, "[]")) {
    internal_delim <- has_internal_delimiters(string, "[]")
  }
  if (is_delimited(string, "\"")) {
    internal_delim <- has_internal_delimiters(string, "\"")
  }

  if (internal_delim) {
    warning(sprintf("extra delimiters within cell %s of table %s", string, table_label))
  }

  return(internal_delim)
}

## Prepare inputs for SQL -------------------------------------------------------------------------
#'
#' For text input, replaces double " quote delimited strings
#' with single ' quote delimited strings.
#'
#' For sql input, appends alias.
#' Leaves all other strings unchanged.
#'
prep_for_sql <- function(string, alias) {
  if (is_delimited(string, "[]")) {
    return(glue::glue("{alias}.{string}"))
  }

  if (!is_delimited(string, "\"")) {
    return(string)
  }

  string <- gsub("\"", "'", string)
  assert(is_delimited(string, "'"), "faulty delimiter")
  return(string)
}

## Handle cases of summary function ---------------------------------------------------------------
#'
#' A range of summary functions can be chosen for the output
#' e.g. MIN, MAX, SUM, COUNT, EXISTS, DURATION, HISTOGRAM, DISTINCT, MEAN.
#'
#' Each of these needs to be translated into 3 outputs
#'  1. label - the text label for the result
#'  2. value - the numeric value of the result
#'  3. group - which columns the table needs to be grouped by
#'
handle_summary_case <- function(summary_type, proportional, m_label, m_value,
                                m_start_date, m_end_date, p_start_date, p_end_date) {
  # prep
  summary_type <- tolower(summary_type)
  # set proportions
  numerator <- glue::glue(
    "DATEDIFF(DAY,",
    "IIF({m_start_date} < {p_start_date}, {p_start_date}, {m_start_date}),",
    "IIF({m_end_date} < {p_end_date}, {m_end_date}, {p_end_date}))"
  )
  denominator <- glue::glue("DATEDIFF(DAY, {m_start_date}, {m_end_date})")
  prop <- ifelse(proportional, glue::glue("1.0 * (1 + {numerator}) / (1 + {denominator})"), "1.0")

  # defaults
  label <- m_label
  group <- m_label

  # handle cases
  if (summary_type == "min") {
    value <- glue::glue("MIN({prop} * {m_value})")
  } else if (summary_type == "max") {
    value <- glue::glue("MAX({prop} * {m_value})")
  } else if (summary_type == "sum") {
    value <- glue::glue("SUM({prop} * {m_value})")
  } else if (summary_type == "count") {
    if (proportional) {
      warning("proportional = TRUE ignored when summarising by 'COUNT'")
    }
    value <- glue::glue("COUNT({m_value})")
  } else if (summary_type == "exists") {
    if (proportional) {
      warning("proportional = TRUE ignored when summarising by 'EXISTS'")
    }
    value <- glue::glue("IIF( COUNT({m_value}) >= 1, 1, 0)")
  } else if (summary_type == "duration") {
    if (proportional) {
      value <- glue::glue("SUM(1 + {numerator})")
    } else {
      value <- glue::glue("SUM(1 + DATEDIFF(DAY, {m_start_date}, {m_end_date}))")
    }
  } else if (summary_type == "histogram") {
    if (proportional) {
      warning("proportional = TRUE ignored when summarising by 'HISTOGRAM'")
    }
    label <- glue::glue("CONCAT({m_label},'=',{m_value})")
    value <- glue::glue("COUNT({m_value})")
    group <- c(m_label, m_value)
  } else if (summary_type == "distinct") {
    if (proportional) {
      warning("proportional = TRUE ignored when summarising by 'DISTINCT'")
    }
    value <- glue::glue("COUNT(DISTINCT {m_value})")
  } else if (summary_type == "mean") {
    value <- glue::glue("AVG({prop} * {m_value})")
  } else {
    assert(FALSE, "unrecognised summary type")
  }

  return(list(label = label, value = value, group = group))
}

## log execution of assembly tool -----------------------------------------------------------------
#'
#'
log_running_of_tool <- function(msg) {
  log_dir <- paste0(ABSOLUTE_PATH_TO_TOOL, "/logs")
  log_file <- paste0(ABSOLUTE_PATH_TO_TOOL, "/logs/TADA_run_log.csv")

  # create directory if it does not exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir)
  }

  # create log file + headers if it does not exist
  if (!file.exists(log_file)) {
    header_list <- c(
      "log_time",
      "msg",
      "ABSOLUTE_PATH_TO_TOOL",
      "ABSOLUTE_PATH_TO_ANALYSIS",
      "POPULATION_FILE",
      "MEASURES_FILE",
      "OUTPUT_DATABASE",
      "OUTPUT_SCHEMA",
      "LONG_THIN_TABLE_NAME",
      "RECTANGULAR_TABLE_NAME",
      "OVERWRITE_EXISTING_TABLES",
      "DEVELOPMENT_MODE",
      "RUN_CHECKS_ONLY",
      "INFO_TO_PRINT_TO_CONSOLE",
      "hash_automated_tests",
      "hash_DHF",
      "hash_GATF",
      "hash_GAT",
      "hash_TCC",
      "hash_UF"
    )
    cat(paste0(header_list, collapse = ","), file = log_file, sep = "\n")
  }

  # gather log contents
  code_hashes <- tools::md5sum(list.files(path = ABSOLUTE_PATH_TO_TOOL, pattern = "\\.R$", full.names = TRUE))
  log_time <- format(Sys.time(), "%Y-%m-%d %H%M")
  log_contents <- c(
    log_time,
    msg,
    ABSOLUTE_PATH_TO_TOOL,
    ABSOLUTE_PATH_TO_ANALYSIS,
    POPULATION_FILE,
    MEASURES_FILE,
    OUTPUT_DATABASE,
    OUTPUT_SCHEMA,
    LONG_THIN_TABLE_NAME,
    RECTANGULAR_TABLE_NAME,
    OVERWRITE_EXISTING_TABLES,
    DEVELOPMENT_MODE,
    RUN_CHECKS_ONLY,
    INFO_TO_PRINT_TO_CONSOLE,
    code_hashes[grepl("automated_tests\\.R$", names(code_hashes))],
    code_hashes[grepl("dbplyr_helper_functions\\.R$", names(code_hashes))],
    code_hashes[grepl("general_assembly_tool_functions\\.R$", names(code_hashes))],
    code_hashes[grepl("general_assembly_tool\\.R$", names(code_hashes))],
    code_hashes[grepl("table_consistency_checks\\.R$", names(code_hashes))],
    code_hashes[grepl("utility_functions\\.R$", names(code_hashes))]
  )
  # append status to log
  cat(paste0(log_contents, collapse = ","), file = log_file, sep = "\n", append = TRUE)
}

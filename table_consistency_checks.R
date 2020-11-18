###############################################################################
#' Description: Table condition checks and asserts
#'
#' Input:
#'
#' Output: Supporting functions for ease of checking tables
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: dplyr, glue, utility_functions.R
#'
#' Notes:
#' - Uses code folding by headers (Alt + O to collapse all)
#' - Many of the functions come in two variants:
#'     check_* variants return TRUE/FALSE
#'     asssert_* variants error on a FALSE, and are silent on TRUE
#' - Functions intended for runtime checks for things we often assume (and should check)
#' - Replicates some existing R functions, but these versions will work on remote tables
#'   as well as local tables.
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-11-18 SA v2 for release
#' 2020-05-04 SA Added checks for number and non-overlapping date periods
#' 2020-04-15 SA Extend unique and join-covered to multiple columns
#' 2020-04-14 SA v1
#' #############################################################################

## Evaluate numeric comparison from string --------------------------------------------------------
#'
#' Provides numerical evaluation of text string comparison
#' E.g. evaluation_direction(14, "<", 10) returns 14<10
#'
evaluation_comparison <- function(val1, direction, val2) {
  assert(is.numeric(val1), "input [val1] must be of type numeric")
  assert(is.numeric(val2), "input [val2] must be of type numeric")

  dd <- direction

  assert(direction %in% c("==", "<", ">", "<=", ">=", "!="), "unrecognised direction provided")
  answer <- eval(parse(text = paste(val1, direction, val2)))

  return(answer)
}

## Number of rows ---------------------------------------------------------------------------------
#'
#' Provides the same functionality as nrow but also works on remote tables.
#'
num_row <- function(df) {
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")

  answer <- df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(num = dplyr::n()) %>%
    dplyr::collect() %>%
    unlist(use.names = FALSE)

  return(answer)
}

## Number unique values in column(s) --------------------------------------------------------------
#'
#' Counts the number of unique values in a column (or set of columns).
#' Designed to work with remote and local tables.
#'
num_unique_entries <- function(df, col_name) {
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.character(col_name), "input [col_name] must be of type character")
  assert(all(col_name %in% colnames(df)), "name provided does not match any column names")

  answer <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!dplyr::syms(col_name)) %>%
    dplyr::distinct() %>%
    dplyr::summarise(num_u = dplyr::n()) %>%
    dplyr::collect() %>%
    unlist(use.names = FALSE)

  return(answer)
}

## Number missing values in column ----------------------------------------------------------------
#'
#' Counts the number of missing values in a column (or cumulative across columns).
#' Designed to work with remote and local tables.
#'
num_missing_entries <- function(df, col_name) {
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.character(col_name), "input [col_name] must be of type character")
  assert(all(col_name %in% colnames(df)), "name provided does not match any column names")

  answer <- 0
  for (col in col_name) {
    this_answer <- df %>%
      dplyr::mutate(missing_tmp = ifelse(is.na(!!dplyr::sym(col)), 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(num_u = sum(missing_tmp)) %>%
      dplyr::collect() %>%
      unlist(use.names = FALSE)

    answer <- answer + this_answer
  }

  return(answer)
}

## Check table size -------------------------------------------------------------------------------
#'
#' Returns T/F for whether number of rows in table is in direction relative to size
#'
check_size <- function(df, direction, size) {
  nn <- num_row(df)
  return(evaluation_comparison(nn, direction, size))
}

## Assert table size ------------------------------------------------------------------------------
#'
#' Asserts number of rows in table is in relative to size
#'
assert_size <- function(df, direction, size) {
  msg <- glue::glue("num rows of df = {num_row(df)} is not {direction} {size}")
  assert(check_size(df, direction, size), msg)
}

## Check comparison of table sizes ----------------------------------------------------------------
#'
#' Returns T/F for whether direction applies to number of rows in both tables
#'
check_size_comparison <- function(df1, direction, df2) {
  n1 <- num_row(df1)
  n2 <- num_row(df2)
  return(evaluation_comparison(n1, direction, n2))
}

## Assert comparison of table sizes ---------------------------------------------------------------
#'
#' Asserts direction applies to number of rows in both tables
#'
assert_size_comparison <- function(df1, direction, df2) {
  msg <- glue::glue(
    "num rows of df1 = {num_row(df1)} is not",
    " {direction} num rows of df2 = {num_row(df2)}"
  )
  assert(check_size_comparison(df1, direction, df2), msg)
}

## Check number of uniques ------------------------------------------------------------------------
#'
#' Returns T/F for whether number of unique values in col_name (or a combination of col_names)
#'  of df is in direction relative to size
#'
check_size_uniques <- function(df, col_name, direction, size) {
  uu <- num_unique_entries(df, col_name)
  return(evaluation_comparison(uu, direction, size))
}

## Assert number of uniques -----------------------------------------------------------------------
#'
#' Assert number of unique values in col_name (or a combination of col_names)
#'  of df is in direction relative to size
#'
assert_size_uniques <- function(df, col_name, direction, size) {
  cc <- paste0(col_name, collapse = ", ")
  msg <- glue::glue(
    "number of uniques in {cc} of df = {num_unique_entries(df, col_name)}",
    " is not {direction} {size}"
  )
  assert(check_size_uniques(df, col_name, direction, size), msg)
}

## Check every entry is unique --------------------------------------------------------------------
#'
#' Returns T/F for whether every entry in col_name (or the combination of col_names) is unique
#'
check_all_unique <- function(df, col_name) {
  nn <- num_row(df)
  check_size_uniques(df, col_name, "==", nn)
}

## Assert every entry is unique -------------------------------------------------------------------
#'
#' Asserts every entry in col_name (or the combination of col_names) is unique
#'
assert_all_unique <- function(df, col_name) {
  cc <- paste0(col_name, collapse = ", ")
  msg <- glue::glue("not all entries in {cc} of df are unique")
  assert(check_all_unique(df, col_name), msg)
}

## Check LHS of join is covered by RHS of join ----------------------------------------------------
#'
#' Returns T/F for whether every value in join_col(s) of df1 is found in df2
#' TRUE --> no records in df1 will be lost during an inner join
#'
#' If joining on columns with different names, use a named list. E.g.:
#' join_col = c(col_in_df1 = "col_in_df2")
#'
check_join_covered <- function(df1, df2, join_col) {
  n1 <- num_row(df1)
  n2 <- num_row(dplyr::semi_join(df1, df2, by = join_col))
  return(evaluation_comparison(n1, "==", n2))
}

## Assert LHS of join is covered by RHS of join ---------------------------------------------------
#'
#' Asserts every value in join_col(s) of df1 is found in df2
#' No error --> no records in df1 will be lost during an inner join
#'
assert_join_covered <- function(df1, df2, join_col) {
  cc2 <- paste0(join_col, collapse = ", ")
  cc1 <- ifelse(is.null(names(join_col)), cc2, paste0(names(join_col), collapse = ", "))
  msg <- glue::glue("not covered, some values in {cc1} of df1 do not appear in {cc2} of df2")
  assert(check_join_covered(df1, df2, join_col), msg)
}

## Check number of missings -----------------------------------------------------------------------
#'
#' Returns T/F for whether number of missing values in col_name (or cumulative across col_names)
#' of DF is in direction relative to size
#'
check_size_missing <- function(df, col_name, direction, size) {
  mm <- num_missing_entries(df, col_name)
  return(evaluation_comparison(mm, direction, size))
}

## Assert number of missings ----------------------------------------------------------------------
#'
#' Assert number of missing values in col_name (or cumulative across col_names)
#'  of df is in direction relative to size
#'
assert_size_missing <- function(df, col_name, direction, size) {
  cc <- paste0(col_name, collapse = ", ")
  msg <- glue::glue(
    "number of missings in {cc} of df = {num_missing_entries(df, col_name)}",
    " is not {direction} {size}"
  )
  assert(check_size_missing(df, col_name, direction, size), msg)
}

## Check date periods do not overlap --------------------------------------------------------------
#'
#' Returns T/F for whether any overlap in date columns within groups defined by other columns.
#'
#' If two individuals have date periods that overlap with each other but neither individual
#' as overlapping date periods when considered on their own
#' Then group_by_cols = person_id will return TRUE (no overlap when grouped)
#' but group_by_cols = c() will return FALSE (overlap exists without grouping)
#'
check_no_date_overlap <- function(df, start_date, end_date, group_by_cols) {
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.character(start_date), "input [start_date] must be of type character")
  assert(length(start_date) == 1, "only a single [start_date] may be specified")
  assert(start_date %in% colnames(df), "[start_date] provided does not match any column names")
  assert(is.character(end_date), "input [end_date] must be of type character")
  assert(length(end_date) == 1, "only a single [end_date] may be specified")
  assert(end_date %in% colnames(df), "[end_date] provided does not match any column names")

  if (length(group_by_cols) == 0) {
    group_by_cols <- "ones_tmp"
    df <- df %>%
      dplyr::mutate(ones_tmp = 1)
  }
  assert(is.character(group_by_cols), "input [group_by_cols] must be of type character")
  assert(all(group_by_cols %in% colnames(df)), "name provided does not match any column names")

  df <- df %>%
    dplyr::rename(
      sd_tmp = !!dplyr::sym(start_date),
      ed_tmp = !!dplyr::sym(end_date)
    )

  # first check no common start dates
  common_start <- df %>%
    dplyr::group_by(!!!dplyr::syms(c(group_by_cols, "sd_tmp"))) %>%
    dplyr::summarise(num_s = dplyr::n()) %>%
    dplyr::filter(num_s != 1) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(num_o = dplyr::n()) %>%
    dplyr::collect() %>%
    unlist(use.names = FALSE)

  if (common_start > 0) {
    return(FALSE)
  }

  # second check no common end dates
  common_end <- df %>%
    dplyr::group_by(!!!dplyr::syms(c(group_by_cols, "ed_tmp"))) %>%
    dplyr::summarise(num_e = dplyr::n()) %>%
    dplyr::filter(num_e != 1) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(num_o = dplyr::n()) %>%
    dplyr::collect() %>%
    unlist(use.names = FALSE)

  if (common_end > 0) {
    return(FALSE)
  }

  # third check no overlaps when start & end dates do not match
  number_overlaps <- df %>%
    dplyr::inner_join(df, by = group_by_cols, suffix = c("_x", "_y")) %>%
    dplyr::select(sd_tmp_x, sd_tmp_y, ed_tmp_x, ed_tmp_y) %>%
    dplyr::filter(
      sd_tmp_x <= ed_tmp_y, # overlap check
      sd_tmp_y <= ed_tmp_x,
      sd_tmp_x != sd_tmp_y, # start and end dates do not match
      ed_tmp_x != ed_tmp_y
    ) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(num_o = dplyr::n()) %>%
    dplyr::collect() %>%
    unlist(use.names = FALSE)

  return(number_overlaps == 0)
}

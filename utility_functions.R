###############################################################################
#' Description: Utility functions to support R development
#'
#' Input:
#'
#' Output: Utility functions for common tasks
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies:
#'
#' Notes:
#' - Uses code folding by headers (Alt + O to collapse all)
#'
#' Issues:
#'
#' History (reverse order):
#' 2020-11-18 SA v2 for release
#' 2020-01-09 SA split SQL focused functions off into dbplyr_helper_functions.R
#' 2020-01-06 pivot function tested, unit tests written and confirmed
#' 2019-11-18 making package use explicit
#' 2019-06-13 replaced copy_r_to_sql with recommended solution (past version in HaBiSA)
#' 2019-06-12 combined versions, improved descriptions & clarity
#' 2018-11-05 New connection string function
#' 2018-09-18 removal of reference to sandpit
#' 2018-08-23 SA write_to_sandpit function merged in
#' 2018-04-24 SA v1
#' #############################################################################

## Assert -----------------------------------------------------------------------------------------
#'
#' Used for testing code during execution.
#' Throws an error if condition is not TRUE
#'
assert <- function(condition, msg) {
  # condition must be logical
  if (!is.logical(condition)) {
    msg <- sprintf("expected condition to be logical, received %s instead\n", class(condition))
    stop(msg)
  }
  # check condition and throw error
  if (condition == FALSE) {
    stop(msg)
  }
}

## Inform user with time stamped measure ----------------------------------------------------------
#' Prints to console time of function call followed by msg.
#'
#' Context parameter allows for conditional use,
#' and is compared against print_level parameter.
#'
run_time_inform_user <- function(msg, context = NA, print_level = NA) {
  assert(is.na(context) | context %in% c("all", "details", "heading"), "invalid context parameter")
  assert(is.na(print_level) | print_level %in% c("all", "details", "heading", "none"), "invalid print level parameter")

  # prefix by level
  msg <- dplyr::case_when(
    context == "heading" ~ toupper(msg),
    context == "details" ~ paste0(" -- ", msg),
    context == "all" ~ paste0(" ---- ", msg),
    TRUE ~ msg
  )
  # rank print level and context
  print_level <- dplyr::case_when(
    is.na(print_level) ~ 3,
    print_level == "all" ~ 3,
    print_level == "details" ~ 2,
    print_level == "heading" ~ 1,
    print_level == "none" ~ -1
  )
  context <- dplyr::case_when(
    is.na(context) ~ 1,
    context == "heading" ~ 1,
    context == "details" ~ 2,
    context == "all" ~ 3
  )

  # conditional print to console
  if (context <= print_level) {
    # time
    now <- as.character(Sys.time())
    # display
    cat(now, "|", msg, "\n")
  }
}

## Not In -----------------------------------------------------------------------------------------
#' Negative of %in%
#'
"%not_in%" <- function(x, y) {
  !("%in%"(x, y))
}

## No special characters --------------------------------------------------------------------------
#'
#'  Check input string for absence of special characters
#' Intended to prevent accidental SQL injection
#'
#' No return if input is string and contains no special characters.
#' Errors if input is not string or if string contains special characters
#' or white space
#'
no_special_characters <- function(in_string) {
  msg <- sprintf("%s must be of type character", deparse(substitute(in_string)))
  assert(is.character(in_string), msg)

  msg <- sprintf("%s contains special characters", deparse(substitute(in_string)))
  assert(!grepl("[;:'(){}?]", in_string), msg)

  msg <- sprintf("%s contains white space", deparse(substitute(in_string)))
  assert(!grepl("[[:space:]]", in_string), msg)

  return(NULL)
}

## Check if delimited -----------------------------------------------------------------------------
#'
#' The entries in the input control tables should be delimited as either
#' [] for sql columns or "" for strings
#' This lets us run a check for the right delimiter, e.g.
#' is_delimited(string, "[]")
#' is_delimited(string, "\"")
#'
is_delimited <- function(string, delimiter) {
  assert(is.character(string), "input must be of type character")
  assert(is.character(delimiter), "input must be of type character")

  n_str <- nchar(string)
  n_delim <- nchar(delimiter)

  string_longer_than_delimiters <- n_str > n_delim
  first_char_delimited <- substr(string, 1, 1) == substr(delimiter, 1, 1)
  last_char_delimited <- substr(string, n_str, n_str) == substr(delimiter, n_delim, n_delim)

  return(string_longer_than_delimiters & first_char_delimited & last_char_delimited)
}

## Remove delimiters ------------------------------------------------------------------------------
#'
#' Remove delimiters from text string
#'
remove_delimiters <- function(string, delimiter) {
  assert(is.character(string), "input must be of type character")
  assert(is.character(delimiter), "input must be of type character")

  n_str <- nchar(string)
  n_delim <- nchar(delimiter)

  first_char <- ifelse(substr(string, 1, 1) == substr(delimiter, 1, 1), 2, 1)
  last_char <- ifelse(substr(string, n_str, n_str) == substr(delimiter, n_delim, n_delim), n_str - 1, n_str)

  return(substr(string, first_char, last_char))
}

## Add delimiters ---------------------------------------------------------------------------------
#'
#' Add delimiters to text string
#'
add_delimiters <- function(string, delimiter) {
  assert(is.character(string), "input must be of type character")
  assert(is.character(delimiter), "input must be of type character")

  if (substr(string, 1, 1) != substr(delimiter, 1, 1)) {
    string <- paste0(substr(delimiter, 1, 1), string)
  }

  n_str <- nchar(string)
  n_delim <- nchar(delimiter)

  if (substr(string, n_str, n_str) != substr(delimiter, n_delim, n_delim)) {
    string <- paste0(string, substr(delimiter, n_delim, n_delim))
  }

  return(string)
}

## Contains additional delimiters -----------------------------------------------------------------
#'
#' Checks delimited string does not contain internal delimiters
#'
has_internal_delimiters <- function(string, delimiter) {
  assert(is.character(string), "input must be of type character")
  assert(is.character(delimiter), "input must be of type character")
  assert(is_delimited(string, delimiter), "string is not delimited")

  if (grepl("\"", delimiter)) {
    delimiter <- paste0(delimiter, "'")
  }

  string <- substr(string, 2, nchar(string) - 1)
  string <- strsplit(string, "") %>% unlist(use.names = FALSE)
  delimiter <- strsplit(delimiter, "") %>% unlist(use.names = FALSE)

  return(any(string %in% delimiter))
}

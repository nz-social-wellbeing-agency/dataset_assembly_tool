###############################################################################
#' Description: Functions for checking confidentialisation rules were applied
#'
#' Input: 
#'
#' Output: Functions for checking confidentialisation
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: dplyr, utility_functions.R, summary_confidential.R
#'
#' Notes:
#' - Designed to handled both local data tables (from csv or Excel files).
#' - Specific long-thin data format used and output by these functions.
#' - Uses code folding by headers (Alt + O to collapse all).
#'
#' Issues:
#'
#' History (reverse order):
#' 2022-08-17 SA v0
#' ############################################################################

## check rounded to specified base (array) ------------------------------------
#' 
#' Checks whether a provided array only contains values that are rounded to
#' the specified base.
#' 
#' If na.rm = TRUE (default) then ignores missing (NA) values.
#' Warns if all values are missing (NA)
#' Errors on non-numeric input.
#' 
check_rounding_to_base_array <- function(input_array, base = 3, na.rm = TRUE){
  assert(is.numeric(base), "base must be numeric")
  assert(is.logical(na.rm), "na.rm must be logical")
  # require input array has length
  assert(length(input_array) > 0, "no input values")
  # check numeric
  assert(all(is.numeric(input_array), na.rm = TRUE), "input is non-numeric")
  
  # warn if only NA's
  if(length(input_array) == sum(is.na(input_array))){
    warning("all input values are NA")
  }
  
  return(all(input_array %% base == 0, na.rm = na.rm))
}

## check rounded to specified base (dataframe) --------------------------------
#' 
#' Checks whether a provided column in a provided dataframe only contains values
#' that are rounded to the specified base.
#' 
#' If na.rm = TRUE (default) then ignores missing (NA) values.
#' Warns if all values are missing (NA)
#' Errors on non-numeric input.
#' 
check_rounding_to_base_df <- function(df, conf_col, base = 3, na.rm = TRUE){
  # df is a dataframe
  assert(is.tbl(df) | is.data.frame(df), "df is not dataset")
  # df is a local dataset (not remote)
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  # conf_col is part of df
  assert(is.character(conf_col), "conf_col must be of type character")
  assert(conf_col %in% colnames(df), "conf_col is not a column name of df")
  
  # run checks
  col_to_array = df[[conf_col]]
  
  return(check_rounding_to_base_array(col_to_array, base = base, na.rm = na.rm))
}

## check random rounding ------------------------------------------------------
#' 
check_random_rounding <- function(df, raw_col, conf_col, base = 3){
}

## suppress small counts ------------------------------------------------------
#'
check_suppression_of_small_counts <- function(df, suppress_cols, threshold, count_cols = suppress_cols){
}

## check for absence of zero counts
#'
check_absence_of_zero_counts <- function(df){
  
  has_long_thin_format(df)
  
}

## confidentialise results ----------------------------------------------------
#' 
check_confidentialised_results <- function(df,
                                    stable_RR = FALSE,
                                    sum_RR = FALSE,
                                    BASE = 3,
                                    COUNT_THRESHOLD = 6,
                                    SUM_THRESHOLD = 20){
  
  assert(has_long_thin_format(df), msg)
  
}

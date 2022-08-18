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
#' Returns TRUE is the provided array only contains values that are rounded to
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
#' Returns TRUE if the provided column in the dataframe only contains values
#' that are rounded to the specified base.
#' 
#' If na.rm = TRUE (default) then ignores missing (NA) values.
#' Warns if all values are missing (NA)
#' Errors on non-numeric input.
#' 
check_rounding_to_base_df <- function(df, column, base = 3, na.rm = TRUE){
  # df is a dataframe
  assert(is.tbl(df) | is.data.frame(df), "df is not dataset")
  # df is a local dataset (not remote)
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  # column is part of df
  assert(is.character(column), "column must be of type character")
  assert(column %in% colnames(df), "column is not a column name of df")
  
  # run checks
  col_to_array = df[[column]]
  
  return(check_rounding_to_base_array(col_to_array, base = base, na.rm = na.rm))
}

## check random rounding ------------------------------------------------------
#' 
#' Returns TRUE if the provided column in the dataframe has been rounded to the
#' specified base.
#' 
#' If a raw column is provided then also checks the randomness of the rounding.
#' - Warns if a raw/unrounded column is not provided.
#' - Warns if rounding does not appear to be random.
#' - Warns if difference between raw and rounded columns is too large.
#' 
check_random_rounding <- function(df, raw_col = NA, conf_col, base = 3){
  # df is a local dataframe
  assert(is.tbl(df) | is.data.frame(df), "df is not dataset")
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  # columns are part of df
  assert(is.character(raw_col) | is.na(raw_col), "raw column must be of type character")
  assert(raw_col %in% colnames(df) | is.na(raw_col), "raw column is not a column name of df")
  assert(is.character(conf_col), "conf column must be of type character")
  assert(conf_col %in% colnames(df), "conf column is not a column name of df")
  
  # check rounding
  rounded_to_base = check_rounding_to_base_df(df, conf_col, base = base)
  
  # warn if no raw column
  if(is.na(raw_col)){
    warning("No raw column provided.\nRandomness of rounding not checked")
    return(rounded_to_base)
  }
  
  rounding_diff = df[[raw_col]] - df[[conf_col]]
  rounding_diff = rounding_diff[!is.na(rounding_diff)]
  
  # warn if difference is too large for rounding
  if(max(abs(rounding_diff)) > base){
    warning("rounding not random - difference exceeds the base")
  }
  
  # warn if all rounding up or all rounding down
  if(all(rounding_diff >= 0)){
    warning("rounding not random - all values rounded up")
  }
  if(all(rounding_diff <= 0)){
    warning("rounding not random - all values rounded down")
  }
  
  # table of ratios of each difference amount
  diff_df = data.frame(rounding = 0:(base - 1)) %>%
    dplyr::mutate(actual = table(abs(rounding_diff))) %>%
    dplyr::mutate(actual = case_when(
      rounding == 0 ~ actual / length(rounding_diff),
      TRUE ~ actual / length(rounding_diff[rounding_diff != 0])
    )) %>%
    dplyr::mutate(expected = case_when(
      rounding == 0 ~ 1 / base,
      TRUE ~ (base - rounding) / ((base - 1) * base / 2)
    )) %>%
    mutate(ratio = actual / expected)
  # warn if distribution of rounding is not expected pattern
  # allows a 20% variation
  if(!all(1 - 0.2 < diff_df$ratio & diff_df$ratio < 1+ 0.2)){
    print(diff_df)
    warning("rounding not random - actual proportions differ from expected by too much")
  }
  
  return(rounded_to_base)
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

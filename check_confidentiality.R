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

## check rounded to specified base --------------------------------------------
#'
check_rounding_to_base_array <- function(input_vector, base = 3){
}

## check rounded to specified base --------------------------------------------
#'
check_rounding_to_base_df <- function(df, conf_col, base = 3){
  
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

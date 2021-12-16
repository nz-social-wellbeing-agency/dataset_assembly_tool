###############################################################################
#' Description: Wrapper for data explorer
#'
#' Input: 
#'
#' Output: Functions to make it faster to generate overviews of a dataset
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: explore and dplyr packages, utility_functions.R
#'
#' Notes:
#' - Designed to handled both local and remote data tables
#' - Uses code folding by headers (Alt + O to collapse all)
#'
#' Issues:
#'
#' History (reverse order):
#' 2021-08-18 SA v0
#' ############################################################################

## Filter to limited number of rows -------------------------------------------
#'
#' Given a dataframe filter it to the specified maximum number of rows.
#' Expected use case is sampling data for exploration or to minimise run time
#' of calculations during development.
#' 
#' Designed to work with both local and remote dataframes.
#'  
#' Logic:
#' - If no id_column is specified, then return the first rows
#' - If an id_column is specified, then select a random sample that will
#'   return approximately the limit number of rows. This is done via modulus
#'   or an ID number.
#' 
#' Assumes that id_column is independent of data. filter is likely to produce
#' non-representative results if id_column is not random with respect to data.
#'  
filter_to_limited_number_of_rows <- function(df, row_limit = 10000, id_column = NA){
  # checks
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.numeric(row_limit), "input [row_limit] must be of type numeric")
  assert(is.na(id_column) || is.character(id_column), "input [id_column] must be provided as a string")
  assert(is.na(id_column) || id_column %in% colnames(df), "[id_column] is not a column name of [df]")
  
  # number of rows (from table_consistency_checks.R)
  number_rows <- df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(num = dplyr::n()) %>%
    dplyr::collect() %>%
    unlist(use.names = FALSE)

  # filter if necessary
  if(number_rows > row_limit & !is.na(id_column)){
    mod_value = ceiling(number_rows / row_limit)
    
    df = df %>%
      filter(!!sym(id_column) %% mod_value == 0)
  }
  
  # output
  output = df %>%
    head(row_limit) %>%
    collect()
}
  
## Explore dataset with report ------------------------------------------------
#'
#' Provide a wrapper for the explore::report to simplify use.
#' Especially for large tables and remote datasets.
#'
explore_report <- function(df, id_column = NA, target = NA, output_file = NA, output_dir = NA){
  # checks
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.na(id_column) || is.character(id_column), "input [id_column] must be provided as a string")
  assert(is.na(target) || is.character(target), "input [target] must be provided as a string")
  assert(is.na(output_file) || is.character(output_file), "input [output_file] must be provided as a string")
  assert(is.na(output_dir) || is.character(output_dir), "input [output_dir] must be provided as a string")
  assert(is.na(target) || target %in% colnames(df), "[target] is not a column name of [df]")
  
  # filter
  df = filter_to_limited_number_of_rows(df = df, row_limit = 10000, id_column = id_column)
  
  # handle NAs
  clean_time = format(Sys.time(), "%Y-%m-%d %H%M%S")
  output_dir = ifelse(is.na(output_dir), getwd(), output_dir)
  output_file = ifelse(is.na(output_file), "df explored", output_file)
  output_file = paste(clean_time, output_file)
  
  # explore
  if(is.na(target)){
    sink("tmp")
	on.exit(sink())
    explore::report(data = df, output_file = output_file, output_dir = output_dir)
  } else {
    sink("tmp")
	on.exit(sink())
    explore::report(data = df, target = !!sym(target), output_file = output_file, output_dir = output_dir)
  }
  unlink("tmp")
  return(paste0(output_file,".html"))
}

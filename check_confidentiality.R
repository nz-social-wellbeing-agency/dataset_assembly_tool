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
#' Else returns FALSE.
#' Warns if all values are missing (NA)
#' Errors on non-numeric input.
#' 
check_rounding_to_base_array <- function(input_array, base = 3, na.rm = TRUE){
  assert(is.numeric(base), "base must be numeric")
  assert(is.logical(na.rm), "na.rm must be logical")
  # require input array has length
  assert(length(input_array) > 0, "no input values")
  # check numeric
  assert(all(is.numeric(input_array) | is.logical(input_array), na.rm = TRUE), "input is non-numeric")
  
  # warn if only NA's
  if(length(input_array) == sum(is.na(input_array))){
    warning("all input values are NA")
  }
  
  result = all(input_array %% base == 0, na.rm = na.rm)
  
  return(ifelse(is.na(result), FALSE, result))
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
  assert(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df), "df is not dataset")
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
check_random_rounding <- function(df, raw_col = NA, conf_col, base = 3, print_ratios = FALSE){
  # df is a local dataframe
  assert(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df), "df is not dataset")
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  # columns are part of df
  assert(is.character(raw_col) | is.na(raw_col), "raw_col must be of type character")
  assert(raw_col %in% colnames(df) | is.na(raw_col), "raw_col is not a column name of df")
  assert(is.character(conf_col), "conf_col must be of type character")
  assert(conf_col %in% colnames(df), "conf_col is not a column name of df")
  
  # check rounding
  rounded_to_base = check_rounding_to_base_df(df, conf_col, base = base)
  
  # warn if no raw column
  if(is.na(raw_col)){
    warning("No raw column provided.\nRandomness of rounding not checked")
    return(rounded_to_base)
  }
  
  rounding_diff = df[[conf_col]] - df[[raw_col]]
  rounding_diff = rounding_diff[!is.na(rounding_diff)]
  rounding_diff = rounding_diff[rounding_diff != 0]
  
  # warn if difference is too large for rounding
  if(any(abs(rounding_diff) >= base)){
    warning("rounding not random - difference exceeds the base")
    return(rounded_to_base)
  }
  
  # warn if all rounding up or all rounding down
  if(all(rounding_diff >= 0)){
    warning("rounding not random - all values rounded up")
    return(rounded_to_base)
  }
  if(all(rounding_diff <= 0)){
    warning("rounding not random - all values rounded down")
    return(rounded_to_base)
  }
  
  # table of ratios of each difference amount
  
  diff_df = data.frame(round_by = 1:(base - 1))
  diff_df$count = sapply(
    diff_df$round_by,
    function(x){ sum(abs(rounding_diff) == x) }
  )
  diff_df$actual_perc = diff_df$count / length(rounding_diff)
  diff_df$expect_perc = (base - diff_df$round_by) / ((base - 1) * base / 2)
  diff_df$ratio = diff_df$actual_perc / diff_df$expect_perc
  
  # warn if distribution of rounding is not expected pattern
  # allows a 20% variation
  # ignores cases where fewer than 5 observations
  if(!all(diff_df$count < 5 | (1 - 0.2 <= diff_df$ratio & diff_df$ratio <= 1 + 0.2))){
    warning("rounding not random - actual proportions differ from expected by too much")
  }
  if(print_ratios){
    print(diff_df)
  }
  
  return(rounded_to_base)
}

## check suppression of small counts ------------------------------------------
#' 
#' Returns TRUE if the provided column in the dataframe has no values where the
#' count is less than the threshold.
#' 
#' We expect: suppressed_col = NA if count_col < threshold.
#' Threshold is the smallest acceptable count.
#'  
check_small_count_suppression <- function(df, suppressed_col, threshold, count_col = suppressed_col){
  # df is a local dataframe
  assert(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df), "df is not dataset")
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  # columns are part of df
  assert(is.character(suppressed_col), "suppressed_col must be of type character")
  assert(suppressed_col %in% colnames(df), "suppressed_col is not a column name of df")
  assert(is.character(count_col), "count_col must be of type character")
  assert(count_col %in% colnames(df), "count_col is not a column name of df")
  # numeric threshold
  assert(is.numeric(threshold), "threshold must be of type numeric")
  
  # check suppression
  suppression_required = df[[count_col]] < threshold
  suppression_required = ifelse(is.na(suppression_required), TRUE, suppression_required)
  suppressed_vals = df[[suppressed_col]][suppression_required]
  return(all(is.na(suppressed_vals)))
}

## check for absence of zero counts -------------------------------------------
#' 
#' If small non-zero counts are suppressed and zero counts do not appear in the
#' dataset, then there is a confidentiality risk that true zeros could be
#' recovered because they are handled differently from small non-zero counts.
#' 
#' Returns TRUE if the absence of zero counts does not pose a confidentiality
#' risk - either there are no suppressed counts OR all combinations of
#' labels/groups appear in the dataset.
#' 
#' As a summarised dataset may be produced by appending multiple summaries
#' we run the analysis within each combination.
#' 
#' If print_on_fail = TRUE then at least one combination that is absent from
#' the dataset is printed.
#' 
#' If a risk is identified (function returns FALSE), there are
#' two common solutions:
#'   1) remove all rows that contain suppression of small non-zero counts
#'   2) use expand_to_include_zero_counts to add zero count rows into dataset
#' 
check_absence_of_zero_counts <- function(df, conf_count_col, print_on_fail = FALSE){
  # df is a local dataframe in required format
  assert(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df), "df is not dataset")
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  assert(has_long_thin_format(df), "output not long-thin formatted as expected")
  # columns are part of df
  assert(is.character(conf_count_col), "conf_count_col must be of type character")
  assert(conf_count_col %in% colnames(df), "conf_count_col is not a column name of df")
  assert(is.logical(print_on_fail), "print_on_fail must be of type logical")
  
  column_names = colnames(df)
  # column groups
  col00 = column_names[grepl("^col[0-9][0-9]$", column_names)]
  val00 = column_names[grepl("^val[0-9][0-9]$", column_names)]
  summary_var = "summarised_var"
  
  # all sub-summaries with the df
  subsummaries = df %>%
    dplyr::select(all_of(c(col00, summary_var))) %>%
    dplyr::distinct()
  
  # iterate through all sub-summaries
  for(ii in 1:nrow(subsummaries)){
    # create sub-summary dataset
    this_subsummary = df %>%
      dplyr::semi_join(subsummaries[ii,], by = c(col00, summary_var))
    
    # if no NA values then there are no concerns - go to next sub-summary
    if(!any(is.na(this_subsummary[[conf_count_col]]))){ next }
    
    # create comparison with all rows
    all_row_df = this_subsummary %>%
      tidyr::expand(!!!syms(c(col00,val00, summary_var)))
    # combinations missing from current sub-summary
    missing_rows = all_row_df %>%
      dplyr::anti_join(this_subsummary, by = c(col00,val00, summary_var))
    
    # if any values missing
    if(nrow(missing_rows) != 0){
      if(print_on_fail){ print(head(missing_rows)) }
      return(FALSE)
    }
  }
  
  # we have checked all sub-summaries and found no concerns
  return(TRUE)
}

## expand to include zero counts ----------------------------------------------
#' 
#' If small non-zero counts are suppressed and zero counts do not appear in the
#' dataset, then there is a confidentiality risk that true zeros could be
#' recovered because they are handled differently from small non-zero counts.
#' 
#' Where a risk is identified (by check_absence_of_zero_counts), there are
#' two common solutions:
#'   1) remove all rows that contain suppression of small non-zero counts
#'   2) use expand_to_include_zero_counts to add zero count rows into dataset
#' 
#' As a summarised dataset may be produced by appending multiple summaries
#' we expand within each combination.
#' 
expand_to_include_zero_counts <- function(df){
  # df is a local dataframe in required format
  assert(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df), "df is not dataset")
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  assert(has_long_thin_format(df), "output not long-thin formatted as expected")
  
  column_names = colnames(df)
  # column groups
  col00 = column_names[grepl("^col[0-9][0-9]$", column_names)]
  val00 = column_names[grepl("^val[0-9][0-9]$", column_names)]
  summary_var = "summarised_var"
  
  col_order = colnames(df)
  
  # all sub-summaries with the df
  subsummaries = df %>%
    dplyr::select(dplyr::all_of(c(col00, summary_var))) %>%
    dplyr::distinct()
  
  expanded_rows_list = lapply(
    1:nrow(subsummaries),
    function(ii){
      # create sub-summary dataset
      this_subsummary = df %>%
        dplyr::semi_join(subsummaries[ii,], by = c(col00, summary_var))
      
      # create comparison with all rows
      all_row_df = this_subsummary %>%
        tidyr::expand(!!!syms(c(col00,val00, summary_var)))
      # sub-summary with all combinations included
      expanded_rows = this_subsummary %>%
        right_join(all_row_df, by = c(col00,val00, summary_var))
    }
  )
  
  expanded_rows_list %>%
    dplyr::bind_rows() %>%
    dplyr::select(dplyr::all_of(col_order)) %>%
    return()
}

## confidentialise results ----------------------------------------------------
#' 
#' Runs and reports on random rounding, suppression, and handling of zeros
#' using the most common defaults.
#' 
check_confidentialised_results <- function(df,
                                           BASE = 3,
                                           COUNT_THRESHOLD = 6,
                                           SUM_THRESHOLD = 20){
  # df is a local dataframe in required format
  assert(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df), "df is not dataset")
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  assert(has_long_thin_format(df), "output not long-thin formatted as expected")
  assert(is.numeric(BASE), "BASE must be of type numeric")
  assert(is.numeric(COUNT_THRESHOLD), "COUNT_THRESHOLD must be of type numeric")
  assert(is.numeric(SUM_THRESHOLD), "SUM_THRESHOLD must be of type numeric")
  
  # log for output
  log = list(column = c(), check = c(), result = c())
  # record log message
  record_log = function(log, column, check, result){
    log$column = c(log$column, column)
    log$check = c(log$check, check)
    log$result = c(log$result, result)
    return(log)
  }
  
  #### distinct ----------------------------------------
  col = "conf_distinct"
  
  chk = glue::glue("checked for RR{BASE}")
  result = tryCatch(
    ifelse(check_random_rounding(df, "raw_distinct", "conf_distinct", BASE), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)
  
  chk = glue::glue("suppressed if raw < {COUNT_THRESHOLD}")
  result = tryCatch(
    ifelse(check_small_count_suppression(df, "conf_distinct", COUNT_THRESHOLD, "raw_distinct"), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)

  #### count ----------------------------------------
  col = "conf_count"
  
  chk = glue::glue("checked for RR{BASE}")
  result = tryCatch(
    ifelse(check_random_rounding(df, "raw_count", "conf_count", BASE), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)

  chk = glue::glue("suppressed if raw < {COUNT_THRESHOLD}")
  result = tryCatch(
    ifelse(check_small_count_suppression(df, "conf_count", COUNT_THRESHOLD, "raw_count"), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)

  #### sum ----------------------------------------
  col = "conf_sum"
  
  chk = glue::glue("suppressed if raw < {SUM_THRESHOLD}")
  result = tryCatch(
    ifelse(
      check_small_count_suppression(df, "conf_sum", SUM_THRESHOLD, "raw_distinct") |
        check_small_count_suppression(df, "conf_sum", SUM_THRESHOLD, "raw_count"), "pass", "fail"
    ),
    error = function(e){ return("skip") }
  )
  log = record_log(log, col, chk, result)

  #### zero counts ----------------------------------------
  
  result = tryCatch(
    ifelse(check_absence_of_zero_counts(df, "conf_count", print_on_fail = FALSE), "pass", "fail"),
    error = function(e){ return("skip") }
  )
  log = record_log(log, "all", "absence of zero counts", result)

  # convert log to formatted message - ensure alignment of checks
  format_string = glue::glue("%{max(nchar(log$column))}s %{max(nchar(log$check))}s : %s")
  msg = sprintf(format_string, log$column, log$check, toupper(log$result)) %>%
    paste0(collapse = "\n")
  
  return(msg)
}

## summarised output overview report ------------------------------------------
#' 
#' Generate overview reports of summarised dataset for research review.
#' Visual inspection of these reports is expected to be part of checks run
#' during delivery.
#' 
#' Note that NA values output in these reports are not reliable. In order to
#' make use of existing reporting functions, we arrange the dataset in a sparse
#' format. The consequence of using a sparse format is that NA we can not
#' distinguish between true NA and formatting NA values.
#' 
explore_output_report <- function(df, output_dir = NA){
  # df is a local dataframe in required format
  assert(tibble::is_tibble(df) | is.data.frame(df) | dplyr::is.tbl(df), "df is not dataset")
  df_classes = tolower(class(df))
  assert(
    !any(sapply(df_classes, grepl, pattern = "sql")),
    "df must be a local dataset"
  )
  assert(has_long_thin_format(df), "output not long-thin formatted as expected")
  
  # column groups
  col00 = grepl("^col[0-9][0-9]$", colnames(df))
  val00 = grepl("^val[0-9][0-9]$", colnames(df))
  summary_var = grepl("^summarised_var$", colnames(df))
  summary = grepl("^(raw_|conf_|)(count|sum|distinct)$", colnames(df))
  
  col00 = colnames(df)[col00]
  val00 = colnames(df)[val00]
  
  #### count ----------------------------------------
  if("conf_count" %in% colnames(df)){
    
    count_list = lapply(
      1:length(col00),
      function(ii){
        # set up iteration key columns
        this_col = col00[ii]
        this_val = val00[ii]
        # set up mutate
        unique_col = unique(df[[this_col]])
        mutate_formula = glue::glue("ifelse(col == '{unique_col}', total_count, NA)")
        mutate_list = as.list(rlang::parse_exprs(mutate_formula))
        names(mutate_list) = unique_col
        
        df %>%
          group_by(!!!syms(c(col00, this_val, "summarised_var"))) %>%
          summarise(total_count = sum(conf_count), .groups = "drop") %>%
          select(col = !!sym(this_col), val = !!sym(this_val), "total_count") %>%
          mutate(!!! mutate_list) %>%
          select(-col, -total_count) %>%
          mutate(val = as.character(val)) %>%
          return()
      }
    )
    
    count_list = bind_rows(count_list)
    # to do here (1) incorporate summarised_var, (2) more efficient if pivot is moved out
    explore_report(count_list, target = "val", output_file = "review_count", output_dir = output_dir)
  }
  
  ## distinct ----------------------------------------
  if("conf_distinct" %in% colnames(df)){
    
    unique_col = unique(df$summarised_var)
    mutate_formula = glue::glue("ifelse(col == '{unique_col}', conf_distinct, NA)")
    mutate_list = as.list(rlang::parse_exprs(mutate_formula))
    names(mutate_list) = unique_col
    
    distinct_df = df %>%
      select(col = summarised_var, conf_distinct) %>%
      mutate(!!! mutate_list) %>%
      select(-col, -conf_distinct)
    # to do - standard code for this across all three sub-sections
    
    explore_report(distinct_df, output_file = "review_distinct", output_dir = output_dir)
  }
  
  ## sum ----------------------------------------
  if(all(c("conf_count", "conf_sum") %in% colnames(df))){
    
    unique_col = unique(df$summarised_var)
    mutate_formula = glue::glue("ifelse(col == '{unique_col}', avg, NA)")
    mutate_list = as.list(rlang::parse_exprs(mutate_formula))
    names(mutate_list) = unique_col
    
    sum_df = df %>%
      mutate(
        conf_sum = as.numeric(conf_sum),
        conf_count = as.numeric(conf_count),
        avg = conf_sum / conf_count
        ) %>%
      select(col = summarised_var, avg) %>%
      mutate(!!! mutate_list) %>%
      select(-col, -avg)
    
    explore_report(sum_df, output_file = "review_averages", output_dir = output_dir)
  }
  
}

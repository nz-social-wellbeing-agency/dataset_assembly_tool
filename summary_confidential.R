###############################################################################
#' Description: Functions for summarising and confidentialising
#'
#' Input: 
#'
#' Output: Functions for summarising and confidentialising datasets
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: dplyr, digest packages, utility_functions.R
#'
#' Notes:
#' - Designed to handled both local and remote data tables.
#' - Specific long-thin data format used and output by these functions.
#' - Uses code folding by headers (Alt + O to collapse all).
#'
#' Issues:
#'
#' History (reverse order):
#' 2022-08-23 SA stable RR3 handles column order
#' 2022-05-20 SA cross product improved
#' 2021-08-18 SA v0
#' ############################################################################

## convert lists to iterator -------------------------------------------
#'
#' Returns a list containing all cross-products of  column names.
#' Intended for grouping by in analysis.
#' 
#' always parameter is included in each output.
#' 
#' drop.dupes.within controls whether duplicated parameters within each set
#' of output columns are discarded.
#' For example: (a,b,c,a,b) becomes (a,b,c).
#' Note: dplyr::group_by requires no duplicates so turning this off
#' may produce errors if the output is used for summarising results.
#' 
#' drop.dupes.across control whether duplicate sets of parameters are discarded.
#' For example: (a,b,c) and (a,c,b) will only output (a,b,c)
#' 
#' For example: always = (a,b,c), grp1 = (d,e), grp2 = (f,g)
#' Returns list containing:
#' (a,b,c,d,f), (a,b,c,d,g), (a,b,c,e,f), (a,b,c,e,g)
#' 
cross_product_column_names <- function(...,
                                       always = NULL,
                                       drop.dupes.within = TRUE,
                                       drop.dupes.across = TRUE){
  # checks
  assert(is.null(always) | is.character(always), "[always] must be character")
  # setup
  groups = list(...)
  output = list(always)
  
  # iterate through all combinations
  for(group in groups){
    new_list = list()
    for(gg in group){
      for(item in output){
        new_list = c(new_list, list(c(item, gg)))
      }
    }
    output = new_list
  }
  
  # drop duplicates
  if(drop.dupes.within){
    new_list = list()
    for(item in output){
      new_list = c(new_list, list(unique(item)))
    }
    output = new_list
  }
  
  # remove duplicates across cross-products (ignores order)
  if(drop.dupes.across){
    new_list = list()
    sort_list = list()
    for(item in output){
      sorted_item = sort(item)
      if(!list(sorted_item) %in% sort_list){
        sort_list = c(sort_list, list(sorted_item))
        new_list = c(new_list, list(item))
      }
    }
    output = new_list
  }
  
  return(output)
}

## check if long-thin format used ---------------------------------------------
#'
#' Checks the our long-thin format and column labels have been used.
#' Formats:
#' col01 val01 ... col99 val99 summarised_var distinct count  sum
#' col01 val01 ... col99 val99 summarised_var raw_distinct raw_count raw_sum conf_distinct conf_count conf_sum
#' 
#' Checks
#' - every column name is col_, val_, distinct, count, or sum
#' - every col_ has a matching val_
#' 
has_long_thin_format <- function(df){
  assert(is.data.frame(df) | dplyr::is.tbl(df), "[df] must be a data.frame")
  
  column_names = colnames(df)
  
  col00 = grepl("^col[0-9][0-9]$", column_names)
  val00 = grepl("^val[0-9][0-9]$", column_names)
  summary_var = grepl("^summarised_var$", column_names)
  summary = grepl("^(raw_|conf_|)(count|sum|distinct)$", column_names)
  
  
  # same number of col and val
  same_number = sum(col00, na.rm = TRUE) == sum(val00, na.rm = TRUE)
  expected_columns = all(col00 | val00 | summary_var | summary)
  
  return(same_number & expected_columns)
}

## random rounding ------------------------------------------------------------
#'
#' Randomly rounds a numeric vector to the given base.
#' For example, if base is 3 (default) then:
#' - 3 & 6 will be left as is because they are already base 3;
#' - 4 & 7 will be rounded down with prob 2/3 and up with prob 1/3;
#' - 5 & 8 will be rounded down with prob 1/3 and up with prob /3.
#'
#' If seeds are provided, these will be used to round the corresponding
#' value in the input vector.
#' 
randomly_round_vector <- function(input_vector, base = 3, seeds = NULL){
  # check vector is numeric
  assert(is.numeric(input_vector), "input [input_vector] must be of type numeric")
  assert(is.numeric(base), "input [base] must be of type numeric")
  
  if(is.null(seeds)){
    probs = runif(length(input_vector))
  } else {
    assert(is.numeric(seeds), "input [seeds] must be of type numeric")
    assert(length(input_vector) == length(seeds), "if used, [seeds] must have same length as [input_vector]")
    probs = sapply(seeds, function(s){set.seed(s); runif(1)})
  }
  
  remainder_vector = input_vector %% base
  prob_round_down = (base - remainder_vector) / base
  ind_round_down = probs < prob_round_down
  
  output_vector = input_vector + base - remainder_vector - base * ind_round_down 
}

## single summarise and label -------------------------------------------------
#'
#' Produces a summary of the specified groups with distinct, count, or sum.
#' Does so in a way that is robust to whether df is a local or a remote table.
#' Returns a local table in long-thin format.
#' 
#' argument clean allows three options:
#' - none = no cleaning
#' - na.as.zero = replaces NA values with zero before summarising
#' - zero.as.na = replaces zero values with NA before summarising
#' NA values are excluded from distinct, count, and sum.
#' 
#' argument remove.na.from.groups determines whether missing values are
#' removed or kept in the grouping columns.
#'
summarise_and_label <- function(df,
                                group_by_cols,
                                summarise_col,
                                make_distinct,
                                make_count,
                                make_sum,
                                clean = "none", # {"none", "na.as.zero", "zero.as.na"}
                                remove.na.from.groups = TRUE){
  #### checks ----
  assert(is.data.frame(df) | dplyr::is.tbl(df), "[df] must be a data.frame")
  assert(is.character(group_by_cols), "[group_by_cols] must be of type character")
  assert(all(group_by_cols %in% colnames(df)), "at least one group column is not a column of [df]")
  assert(is.character(summarise_col), "[summarise_col] must be of type character")
  assert(length(summarise_col) == 1, "only one [summarise_col] can be specified")
  assert(summarise_col %in% colnames(df), "[summarise_col] must be a column of [df]")
  assert(clean %in% c("none", "na.as.zero", "zero.as.na"), "[clean] accepts only three options see documentation")
  assert(is.logical(make_distinct), "[make_distinct] must be type logical")
  assert(is.logical(make_count), "[make_count] must be type logical")
  assert(is.logical(make_sum), "[make_sum] must be type logical")
  
  #### cleaning ----
  df = dplyr::select(df, all_of(c(group_by_cols, summarise_col)))
  
  if(remove.na.from.groups){
    # apply filters in a single step
    tmp = paste0(" !is.na(", group_by_cols, ") ")
    df = dplyr::filter(df, `!!!`(rlang::parse_exprs(tmp)))
    
    ## prev version
    # for(gg in group_by_cols){
    #   df = dplyr::filter(df, !is.na(!!sym(gg)))
    # }
  }
  
  if(clean == "na.as.zero"){
    df = dplyr::mutate(df, !!sym(summarise_col) := ifelse(is.na(!!sym(summarise_col)), 0, !!sym(summarise_col)))
  } else if(clean == "zero.as.na"){
    df = dplyr::mutate(df, !!sym(summarise_col) := ifelse(!!sym(summarise_col) == 0, NA, !!sym(summarise_col)))
  }
  
  #### summarise ----
  output_df = df %>%
    dplyr::select(!!!syms(group_by_cols)) %>%
    dplyr::distinct() %>%
    dplyr::collect()
  
  df = dplyr::group_by(df, !!!syms(group_by_cols))
  
  if(make_distinct){
    tmp_df = df %>%
      dplyr::filter(!is.na(!!sym(summarise_col))) %>%
      dplyr::summarise(distinct = dplyr::n_distinct(!!sym(summarise_col)), .groups = "drop")
    
    # output query if relevant
    if("tbl_sql" %in% class(tmp_df)){
      save_to_sql(tmp_df %>% dbplyr::sql_render() %>% as.character(), "make distinct")
    }
    
    output_df = tmp_df %>%
      dplyr::collect() %>%
      dplyr::right_join(output_df, by = group_by_cols)
  }
  
  if(make_count){
    tmp_df = df %>%
      dplyr::summarise(count = sum(ifelse(is.na(!!sym(summarise_col)), 0, 1), na.rm = TRUE), .groups = "drop")
    
    # output query if relevant
    if("tbl_sql" %in% class(tmp_df)){
      save_to_sql(tmp_df %>% dbplyr::sql_render() %>% as.character(), "make count")
    }
    
    output_df = tmp_df %>%
      dplyr::collect() %>%
      dplyr::right_join(output_df, by = group_by_cols)
  }
  
  if(make_sum){
    tmp_df = df %>%
      dplyr::summarise(sum = sum(!!sym(summarise_col), na.rm = TRUE), .groups = "drop")
    
    # output query if relevant
    if("tbl_sql" %in% class(tmp_df)){
      save_to_sql(tmp_df %>% dbplyr::sql_render() %>% as.character(), "make sum")
    }
    
    output_df = tmp_df %>%
      dplyr::collect() %>%
      dplyr::right_join(output_df, by = group_by_cols)
  }
  
  #### label ----
  # converting to long-thin format
  
  col_order = c()
  
  for(ii in 1:length(group_by_cols)){
    col = sprintf("col%02d", ii)
    val = sprintf("val%02d", ii)
    
    output_df = output_df %>%
      dplyr::mutate(!!sym(col) := group_by_cols[ii]) %>%
      dplyr::rename(!!sym(val) := !!sym(group_by_cols[ii])) %>%
      dplyr::mutate(!!sym(val) := as.character(!!sym(val)))
    
    col_order = c(col_order, col, val)
  }
  
  col_order = c(col_order, "summarised_var")
  if(make_distinct){
    col_order =  c(col_order, "distinct")
  }
  if(make_count){
    col_order = c(col_order, "count")
  }
  if(make_sum){
    col_order = c(col_order, "sum")
  }
  
  output_df = output_df %>%
    dplyr::mutate(summarised_var = summarise_col) %>%
    dplyr::select(!!!syms(col_order))

  #### conclude ----
  assert(has_long_thin_format(output_df), "output not long-thin formatted as expected")
  return(output_df)
}

## summarise and label from list ----------------------------------------------
#'
#' Produces a summary of every variable given in summarise_list for every
#' group given in group_by_list. Allows for distinct, count, or sum summary.
#' 
#' Does so in a way that is robust to whether df is a local or a remote table.
#' Returns a local table in long-thin format.
#' 
#' argument clean allows three options:
#' - none = no cleaning
#' - na.as.zero = replaces NA values with zero before summarising
#' - zero.as.na = replaces zero values with NA before summarising
#' NA values are excluded from distinct, count, and sum.
#' 
#' argument remove.na.from.groups determines whether missing values are
#' removed or kept in the grouping columns.
#'
summarise_and_label_over_lists <- function(df, 
                                           group_by_list,
                                           summarise_list,
                                           make_distinct,
                                           make_count,
                                           make_sum,
                                           clean = "none", # {"none", "na.as.zero", "zero.as.na"}
                                           remove.na.from.groups = TRUE){
  #### checks ----
  assert(is.data.frame(df) | dplyr::is.tbl(df), "[df] must be of type data.frame")
  assert(is.list(group_by_list), "[group_by_list] must be a list of groups")
  assert(length(group_by_list) >= 1, "at least one group must be provided in [group_by_list]")
  assert(is.list(summarise_list), "[summarise_list] must be a list of groups")
  assert(length(summarise_list) >= 1, "at least one group must be provided in [summarise_list]")
  for(ii in 1:length(group_by_list)){
    assert(is.character(group_by_list[[ii]]), glue::glue("group {ii} is not of type character"))
    assert(all(group_by_list[[ii]] %in% colnames(df)), glue::glue("group {ii} requires columns not in [df]"))
  }
  for(ii in 1:length(summarise_list)){
    assert(is.character(summarise_list[[ii]]), glue::glue("summary column {ii} is not of type character"))
    assert(summarise_list[[ii]] %in% colnames(df), glue::glue("summary column {ii} is not found in [df]"))
  }
  
  #### make all combinations ----
  output_list = list()
  
  for(gg in group_by_list){
    for(ss in summarise_list){
      this_df = summarise_and_label(df, gg, ss, make_distinct, make_count, make_sum, clean, remove.na.from.groups)
      
      output_list = c(output_list, list(this_df))
    }
  }

  #### conclude ----
  
  # ensure all val columns are of type character
  output_list = lapply(output_list, function(df){mutate(df, across(starts_with("val"), as.character))})
  
  # list of df's into a single df
  output_df = dplyr::bind_rows(output_list)
  
  assert(has_long_thin_format(output_df), "output not long-thin formatted as expected")
  return(output_df)
}

## apply random rounding ------------------------------------------------------
#'
#' Applied random rounding to specified columns.
#' Creates raw_ and conf_ columns so original values are preserved.
#' 
#' If stable_across_cols are provided, then these will be used to generate
#' seeds for random rounding to ensure that rounding is stable between 
#' repeated use of this function. Increases run time.
#' 
apply_random_rounding <- function(df, RR_columns, BASE = 3, stable_across_cols = NULL){
  # checks
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.numeric(BASE), "input [BASE] must be of type numeric")
  assert(all(RR_columns %in% colnames(df)), "[RR_columns] must be column names of [df]")
  # check stable_across_cols if provided
  if(!is.null(stable_across_cols)){
    assert(all(stable_across_cols %in% colnames(df)), "[stable_across_cols] must be column names of [df]")
    assert(!any(stable_across_cols %in% RR_columns), "[stable_across_cols] should not be found in [RR_columns]")
  }
  
  # loop through column types
  for(this_col in RR_columns){
    # value for this iteration
    raw_col = paste0("raw_", this_col)
    conf_col = paste0("conf_", this_col)
    
    # make raw_* column
    df = dplyr::rename(df, !!sym(raw_col) := !!sym(this_col))
    # make conf_* column
    if(is.null(stable_across_cols)){
      # round
      df = dplyr::mutate(df, !!sym(conf_col) := randomly_round_vector(!!sym(raw_col), base = BASE))
    } else {
      # make seeds
      sort_remove_nas = function(x){
        x = unname(x[!is.na(x)])
        return(paste(sort(x), collapse = " "))
      }
      concated = dplyr::select(df, dplyr::all_of(stable_across_cols)) %>%
        apply(MARGIN = 1, sort_remove_nas)
      
      df$tmp_concatenated = concated
      df$tmp_hashed = sapply(df$tmp_concatenated, digest::digest)  
      df = dplyr::mutate(df, tmp_seed = digest::digest2int(tmp_hashed))
      # round
      df = df %>%
        dplyr::mutate(!!sym(conf_col) := randomly_round_vector(!!sym(raw_col), base = BASE, seeds = tmp_seed)) %>%
        select(-tmp_concatenated, -tmp_seed, -tmp_hashed)
    }
    
  } # end for loop

  return(df)
}

## apply graduated random rounding --------------------------------------------
#'
#' Applied graduated random rounding (GRR) to specified columns.
#' Creates raw_ and conf_ columns so original values are preserved.
#' 
#' If stable_across_cols are provided, then these will be used to generate
#' seeds for random rounding to ensure that rounding is stable between 
#' repeated use of this function. Increases run time.
#' 
#' Thresholds for graduation are set by the Stats NZ microdata output guide.
#' Only apply one of GRR or RR3. Applyng both will cause errors.
#' 
apply_graduated_random_rounding <- function(df, GRR_columns, stable_across_cols = NULL){
  # checks
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(all(GRR_columns %in% colnames(df)), "[GRR_columns] must be column names of [df]")
  # check stable_across_cols if provided
  if(!is.null(stable_across_cols)){
    assert(all(stable_across_cols %in% colnames(df)), "[stable_across_cols] must be column names of [df]")
    assert(!any(stable_across_cols %in% GRR_columns), "[stable_across_cols] should not be found in [GRR_columns]")
  }
  
  # loop through column types
  for(this_col in GRR_columns){
    # value for this iteration
    raw_col = paste0("raw_", this_col)
    conf_col = paste0("conf_", this_col)
    
    # make raw_* column
    df = dplyr::rename(df, !!sym(raw_col) := !!sym(this_col))
    # make conf_* column
    if(is.null(stable_across_cols)){
      # round
      df = dplyr::mutate(df, !!sym(conf_col) := case_when(
        0 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 19 ~ 
          randomly_round_vector(!!sym(raw_col), base = 3),
        19 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 20 ~ 
          randomly_round_vector(!!sym(raw_col), base = 2),
        20 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 100 ~ 
          randomly_round_vector(!!sym(raw_col), base = 5),
        100 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 1000 ~ 
          randomly_round_vector(!!sym(raw_col), base = 10),
        1000 <= abs(!!sym(raw_col)) ~ 
          randomly_round_vector(!!sym(raw_col), base = 100)
      ))
    } else {
      # make seeds
      sort_remove_nas = function(x){
        x = unname(x[!is.na(x)])
        return(paste(sort(x), collapse = " "))
      }
      concated = dplyr::select(df, dplyr::all_of(stable_across_cols)) %>%
        apply(MARGIN = 1, sort_remove_nas)
      
      df$tmp_concatenated = concated
      df$tmp_hashed = sapply(df$tmp_concatenated, digest::digest)  
      df = dplyr::mutate(df, tmp_seed = digest::digest2int(tmp_hashed))
      # round
      df = df %>%
        dplyr::mutate(!!sym(conf_col) := case_when(
          0 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 19 ~ 
            randomly_round_vector(!!sym(raw_col), base = 3, seeds = tmp_seed),
          19 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 20 ~ 
            randomly_round_vector(!!sym(raw_col), base = 2, seeds = tmp_seed),
          20 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 100 ~ 
            randomly_round_vector(!!sym(raw_col), base = 5, seeds = tmp_seed),
          100 <= abs(!!sym(raw_col)) & abs(!!sym(raw_col)) < 1000 ~ 
            randomly_round_vector(!!sym(raw_col), base = 10, seeds = tmp_seed),
          1000 <= abs(!!sym(raw_col)) ~ 
            randomly_round_vector(!!sym(raw_col), base = 100, seeds = tmp_seed)
        )) %>%
        select(-tmp_concatenated, -tmp_seed, -tmp_hashed)
    }
    
  } # end for loop
  
  return(df)
}

## suppress small counts ------------------------------------------------------
#'
#' Suppresses values where the count is too small.
#' Values in suppress_cols are replaced with NA if the corresponding value
#' in count_cols is less than the threshold. count_cols is assumed to contain
#' counts of unit records.
#' 
#' If multiple columns are provided then values are suppressed if any count
#' is beneath the threshold.
#'
apply_small_count_suppression <- function(df, suppress_cols, threshold, count_cols = suppress_cols){
  # checks
  assert(is.data.frame(df) | dplyr::is.tbl(df), "input [df] must be of type data.frame")
  assert(is.numeric(threshold), "input [threshold] must be of type numeric")
  assert(all(suppress_cols %in% colnames(df)), "[suppress_cols] must be column names of [df]")
  assert(all(count_cols %in% colnames(df)), "[count_cols] must be column names of [df]")
  
  # table of count/threshold values (handled separate threshold per cout column)
  threshold_tbl = data.frame(col = count_cols, threshold = threshold, stringsAsFactors = FALSE)
  
  # suppress
  for(ii in 1:nrow(threshold_tbl)){
    ref = c(threshold_tbl[ii,])
    
    ind_suppress = df[[ref$col]] < ref$threshold
    ind_suppress = sapply(ind_suppress, isTRUE)
    df[ind_suppress, suppress_cols] = NA
  }
  
  return(df)
}

## confidentialise results ----------------------------------------------------
#'
#' Accepts a table in long-thin format output by summarise and label and
#' applies confidentialisation rules:
#' - random rounding 
#' - suppression of small values
#' - ensures consistency between RR and suppression
#' 
confidentialise_results <- function(df,
                                    stable_RR = FALSE,
                                    sum_RR = FALSE,
                                    BASE = 3,
                                    COUNT_THRESHOLD = 6,
                                    SUM_THRESHOLD = 20){
  # checks
  assert(has_long_thin_format(df), "[df] is not in expected format")
  assert(any(c("distinct", "count") %in% colnames(df)), "[df] must have column distinct, count, or both")
  assert(is.logical(stable_RR), "[stable_RR] must be type logical")
  assert(is.logical(sum_RR), "[sum_RR] must be type logical")

  # stability
  if(stable_RR){
    col00_val00 = grepl("^(col|val)[0-9][0-9]$", colnames(df))
    summary_var = grepl("^summarised_var$", colnames(df))
    stable_across_cols = colnames(df)[col00_val00 | summary_var]
  } else {
    stable_across_cols = NULL
  }
  
  # random rounding
  if(sum_RR){
    RR_columns = c("distinct", "count", "sum")
  } else {
    RR_columns = c("distinct", "count")
  }
  RR_columns = RR_columns[RR_columns %in% colnames(df)]

  df = apply_random_rounding(df = df, RR_columns = RR_columns, BASE = BASE, stable_across_cols = stable_across_cols)
  
  # suppression of counts
  count_cols = c("raw_distinct", "raw_count")
  count_cols = count_cols[count_cols %in% colnames(df)]
  
  num_cols = c("conf_distinct", "conf_count")
  num_cols = num_cols[num_cols %in% colnames(df)]
  
  if(length(num_cols) >= 1){
    df = apply_small_count_suppression(df = df, suppress_cols = num_cols, threshold = COUNT_THRESHOLD, count_cols = count_cols)
  }
  
  # suppression of sums
  if("sum" %in% colnames(df)){
    df = df %>%
      dplyr::mutate(conf_sum = sum) %>%
      dplyr::rename(raw_sum = sum)
  }
  
  if("conf_sum" %in% colnames(df)){
    df = apply_small_count_suppression(df = df, suppress_cols = "conf_sum", threshold = SUM_THRESHOLD, count_cols = count_cols)
    
    # check rounding and suppression consistency
    for(ii in 1:length(num_cols)){
      count_col = dplyr::sym(count_cols[ii])
      num_col = dplyr::sym(num_cols[ii])
      
      # if raw >= threshold and conf < threshold, enforce rounding down so conf >= threshold
      df = dplyr::mutate(df, !!num_col := ifelse(!!num_col < SUM_THRESHOLD & !!count_col >= SUM_THRESHOLD, !!num_col + BASE, !!num_col))
      # if raw < threshold and conf >= threshold, enforce rounding down so conf < threshold
      df = dplyr::mutate(df, !!num_col := ifelse(!!num_col >= SUM_THRESHOLD & !!count_col < SUM_THRESHOLD, !!num_col - BASE, !!num_col))
    }
  }
  
  # conclude
  assert(has_long_thin_format(df), "output not long-thin formatted as expected")
  return(df)
}

###############################################################################
#' Description: Automated tests for checking confidentialisation functions
#'
#' Input: check_confidentiality.R
#'
#' Output: Test pass/fail report
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: testthat package, utility_functions.R
#'
#' Notes:
#'
#' Issues:
#'
#' History (reverse order):
#' 2022-08-22 SA v0
#' #############################################################################

#' Testing the following functions that check output is confidentialiseed
#' 
#' check_rounding_to_base_array(input_array, base = 3, na.rm = TRUE)
#' check_rounding_to_base_df(df, column, base = 3, na.rm = TRUE)
#' check_random_rounding(df, raw_col = NA, conf_col, base = 3)
#' check_small_count_suppression(df, suppressed_col, threshold, count_col = suppressed_col)
#' check_absence_of_zero_counts(df, conf_count_col, print_on_fail = TRUE)
#' expand_to_include_zero_counts(df)
#' check_confidentialised_results(df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
#' explore_output_report(df, output_dir = NA)
#' 
context("check confidentiality - checks")

#####################################################################
test_that("rounding vector passes & fails when expected", {
  # arrange
  vec3 = (1:14) * 3
  vec4 = (1:14) * 4
  vec17 = (27:82) * 17
  vec3na = c(vec3, NA)
  vec4na = c(vec4, NA)
  vec17na = c(vec17, NA)
  
  # act & assert
  expect_true(check_rounding_to_base_array(vec3, base = 3, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec4, base = 4, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec4, base = 2, na.rm = FALSE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = FALSE))
  
  expect_true(check_rounding_to_base_array(vec3, base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4, base = 4, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4, base = 2, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = TRUE))
  
  expect_true(check_rounding_to_base_array(vec3na, base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4na, base = 4, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec4na, base = 2, na.rm = TRUE))
  expect_true(check_rounding_to_base_array(vec17, base = 17, na.rm = TRUE))
  
  # act & assert
  expect_false(check_rounding_to_base_array(vec3na, base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec4na, base = 4, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec4na, base = 2, na.rm = FALSE))
  expect_false(check_rounding_to_base_array(vec17na, base = 17, na.rm = FALSE))
  
  expect_false(check_rounding_to_base_array(1:50, base = 2, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec3, base = 6, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec3na, base = 4, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec4na, base = 3, na.rm = TRUE))
  expect_false(check_rounding_to_base_array(vec17, base = 7, na.rm = TRUE))
})

test_that("rounding vector errors when expected", {
  
  expect_error(check_rounding_to_base_array(1:50, base = "3", na.rm = TRUE), "numeric")
  expect_error(check_rounding_to_base_array(1:50, base = 3, na.rm = "TRUE"), "logical")
  expect_error(check_rounding_to_base_array(c(), base = 3, na.rm = TRUE), "no input")
  expect_error(check_rounding_to_base_array(c(), base = 3, na.rm = FALSE), "no input")
  expect_error(check_rounding_to_base_array(c(), base = 3, na.rm = TRUE), "no input")
  expect_error(check_rounding_to_base_array(c(1,2,3,NA,"NA"), base = 3, na.rm = TRUE), "numeric")
  
  expect_warning(check_rounding_to_base_array(c(NA, NA, NA), base = 3, na.rm = FALSE), "NA")
})

#####################################################################
test_that("rounding df passes & failed when expected", {
  # arrange
  input_df = data.frame(
    rr3 = 3 * 1:10,
    rr6 = 6 * 21:30,
    rrx = 41:50,
    rr3na = c(NA, 3 * 1:9),
    rr6na = c(6 * 61:69, NA)
  )
  
  # act & assert
  expect_true(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6", base = 6, na.rm = TRUE))
  
  expect_false(check_rounding_to_base_df(input_df, "rr3na", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rr6na", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rr6na", base = 6, na.rm = FALSE))
  expect_true(check_rounding_to_base_df(input_df, "rr3na", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6na", base = 3, na.rm = TRUE))
  expect_true(check_rounding_to_base_df(input_df, "rr6na", base = 6, na.rm = TRUE))
  
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 2, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 3, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 5, na.rm = FALSE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 2, na.rm = TRUE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 3, na.rm = TRUE))
  expect_false(check_rounding_to_base_df(input_df, "rrx", base = 5, na.rm = TRUE))
})

test_that("rounding df errors when expected", {
  input_df = data.frame(rr3 = 3 * 1:5, rrx = 6:10, rr3na = c(3,6,3,9,NA), nas = c(NA, NA, NA, NA, NA))
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  
  expect_error(check_rounding_to_base_df("input_df", "rr3", base = 3, na.rm = FALSE), "dataset")
  expect_error(check_rounding_to_base_df(remote_df, "rr3", base = 3, na.rm = FALSE), "local")
  expect_error(check_rounding_to_base_df(input_df, 2, base = 3, na.rm = FALSE), "character")
  expect_error(check_rounding_to_base_df(input_df, "rr13", base = 3, na.rm = FALSE), "column name")
  
  # errors from passing column to vector
  expect_error(check_rounding_to_base_df(input_df, "rr3", base = "3", na.rm = TRUE), "numeric")
  expect_error(check_rounding_to_base_df(input_df, "rr3", base = 3, na.rm = "TRUE"), "logical")
  
  expect_warning(check_rounding_to_base_df(input_df, "nas", base = 3, na.rm = TRUE), "NA")
})

#####################################################################
test_that("random rounding checks pass & fail when expected", {
  # arrange
  input_df = data.frame(
    raw = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
    round_down = c(0,0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21),
    round_up = c(3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21,24,24),
    round_closest = c(0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21),
    round_furthest = c(3,0,3,6,3,6,9,6,9,12,9,12,15,12,15,18,15,18,21,18,21,24),
    round_random1 = c(3,3,3,3,6,6,6,9,9,12,12,12,12,15,15,18,18,18,21,18,21,21),
    round_random2 = c(0,0,3,3,3,6,6,6,9,12,12,12,12,12,15,15,18,18,18,21,21,21),
    round_random3 = c(3,3,3,3,6,6,9,9,9,9,9,12,12,15,15,15,18,18,18,18,21,21)
  )
  
  # act & assert
  expect_true(check_random_rounding(input_df, "raw", "round_random1", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_random2", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_random3", base = 3))
  expect_false(check_random_rounding(input_df, "round_random1", "raw", base = 3))
  
  defaultW <- getOption("warn") 
  options(warn = -1) 
  
  expect_true(check_random_rounding(input_df, "raw", "round_down", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_up", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_closest", base = 3))
  expect_true(check_random_rounding(input_df, "raw", "round_furthest", base = 3))
  
  expect_false(check_random_rounding(input_df, "raw", "raw", base = 3))
  
  expect_true(check_random_rounding(input_df, conf_col = "round_random1", base = 3))
  expect_true(check_random_rounding(input_df, conf_col = "round_random2", base = 3))
  expect_true(check_random_rounding(input_df, conf_col = "round_random3", base = 3))
  expect_true(check_random_rounding(input_df, conf_col = "round_down", base = 3))
  expect_true(check_random_rounding(input_df, conf_col = "round_up", base = 3))
  expect_true(check_random_rounding(input_df, conf_col = "round_closest", base = 3))
  expect_true(check_random_rounding(input_df, conf_col = "round_furthest", base = 3))
  
  expect_false(check_random_rounding(input_df, conf_col = "raw", base = 3))
  
  options(warn = defaultW)
})

test_that("random rounding checks warn & error when expected", {
  # arrange
  input_df = data.frame(
    raw = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
    round_down = c(0,0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21),
    round_up = c(3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21,24),
    round_closest = c(0,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21),
    round_furthest = c(3,0,3,6,3,6,9,6,9,12,9,12,15,12,15,18,15,18,21,18,21,24),
    round_random1 = c(3,3,3,3,6,6,6,9,9,12,12,12,12,15,15,18,18,18,21,18,21,21),
    round_random2 = c(0,0,3,3,3,6,6,6,9,12,12,12,12,12,15,15,18,18,18,21,21,21),
    round_random3 = c(3,3,3,3,6,6,9,9,9,9,9,12,12,15,15,15,18,18,18,18,21,21)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  
  # act & assert
  expect_error(check_random_rounding("input_df", "raw", "round_random1", base = 3), "dataset")
  expect_error(check_random_rounding(remote_df, "raw", "round_random1", base = 3), "local")
  expect_error(check_random_rounding(input_df, 2, "round_random1", base = 3), "character")
  expect_error(check_random_rounding(input_df, "raw_x", "round_random1", base = 3), "column name")
  expect_error(check_random_rounding(input_df, "raw", NA, base = 3), "character")
  expect_error(check_random_rounding(input_df, "raw", "round_random1_x", base = 3), "column name")
  
  expect_warning(check_random_rounding(input_df, conf_col = "round_random1", base = 3), "raw column")
  expect_warning(check_random_rounding(input_df, "round_down", "round_up", base = 3), "difference exceeds")
  expect_warning(check_random_rounding(input_df, "raw", "round_up", base = 3), "rounded up")
  expect_warning(check_random_rounding(input_df, "raw", "round_down", base = 3), "rounded down")
  
  expect_warning(check_random_rounding(input_df, "raw", "round_closest", base = 3), "not random")
  expect_warning(check_random_rounding(input_df, "raw", "round_furthest", base = 3), "not random")
})

#####################################################################
test_that("small count suppression pass & fail when expected", {
  # arrange
  input_df = data.frame(
    raw_count = 1:30,
    conf_count = c(rep_len(NA, 5), 6:30),
    conf_sum = c(rep_len(NA, 19), 20:30 * 100)
  )
  
  # act & assert
  expect_true(check_small_count_suppression(input_df, "conf_count", 6, count_col = "raw_count"))
  expect_true(check_small_count_suppression(input_df, "conf_count", 6))
  expect_true(check_small_count_suppression(input_df, "conf_count", 4, count_col = "raw_count"))
  expect_true(check_small_count_suppression(input_df, "conf_count", 4))
  expect_true(check_small_count_suppression(input_df, "conf_sum", 20, count_col = "raw_count"))
  expect_true(check_small_count_suppression(input_df, "conf_sum", 20, count_col = "conf_count"))
  expect_true(check_small_count_suppression(input_df, "conf_sum", 15, count_col = "raw_count"))
  expect_true(check_small_count_suppression(input_df, "conf_sum", 15, count_col = "conf_count"))
  
  expect_false(check_small_count_suppression(input_df, "conf_count", 8, count_col = "raw_count"))
  expect_false(check_small_count_suppression(input_df, "conf_count", 8))
  expect_false(check_small_count_suppression(input_df, "conf_sum", 22, count_col = "raw_count"))
  expect_false(check_small_count_suppression(input_df, "conf_sum", 22, count_col = "conf_count"))
  expect_false(check_small_count_suppression(input_df, "raw_count", 6, count_col = "conf_count"))
})

test_that("small count suppression error when expected", {
  # arrange
  input_df = data.frame(
    raw_count = 1:30,
    conf_count = c(rep_len(NA, 5), 6:30),
    conf_sum = c(rep_len(NA, 19), 20:30 * 100)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  
  
  check_small_count_suppression(input_df, "conf_count", 6, count_col = "raw_count")
  
  expect_error(check_small_count_suppression("input_df", "conf_count", 6, count_col = "raw_count"), "dataset")
  expect_error(check_small_count_suppression(remote_df, "conf_count", 6, count_col = "raw_count"), "local")
  expect_error(check_small_count_suppression(input_df, NA, 6, count_col = "raw_count"), "character")
  expect_error(check_small_count_suppression(input_df, "conf_count", 6, count_col = 4), "character")
  expect_error(check_small_count_suppression(input_df, "conf_count_x", 6, count_col = "raw_count"), "column name")
  expect_error(check_small_count_suppression(input_df, "conf_count", 6, count_col = "raw_count_x"), "column name")
  expect_error(check_small_count_suppression(input_df, "conf_count", "6", count_col = "raw_count"), "numeric")
})

#####################################################################
test_that("absence of zeroes pass & fail when expected", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    conf_count = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30)
  )
  
  # no NAs
  expect_true(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count"))
  
  for(ii in 1:nrow(input_df)){
    tmp_df = dplyr::filter(input_df, dplyr::row_number() != ii)
    expect_true(check_absence_of_zero_counts(tmp_df, conf_count_col = "conf_count"))
  }
  
  # with NAs
  input_df$conf_count[1:2] = NA
  expect_true(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count"))
  
  for(ii in 1:nrow(input_df)){
    tmp_df = dplyr::filter(input_df, dplyr::row_number() != ii)
    if(ii %in% 1:6){
      expect_false(check_absence_of_zero_counts(tmp_df, conf_count_col = "conf_count"))
    } else {
      expect_true(check_absence_of_zero_counts(tmp_df, conf_count_col = "conf_count"))
    }
  }
  
  # with NAs 2
  input_df$conf_count[15:16] = NA
  expect_true(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count"))
  
  for(ii in 1:nrow(input_df)){
    tmp_df = dplyr::filter(input_df, dplyr::row_number() != ii)
    expect_false(check_absence_of_zero_counts(tmp_df, conf_count_col = "conf_count"))
  }
})

test_that("absence of zeroes errors when expected", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    conf_count = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  alt_df = dplyr::rename(input_df, wrong_col_name = col01)
  
  # act & assert
  expect_error(check_absence_of_zero_counts("input_df", conf_count_col = "conf_count"), "dataset")
  expect_error(check_absence_of_zero_counts(remote_df, conf_count_col = "conf_count"), "local")
  expect_error(check_absence_of_zero_counts(alt_df, conf_count_col = "conf_count"), "long-thin")
  expect_error(check_absence_of_zero_counts(input_df, conf_count_col = 4), "character")
  expect_error(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count_x"), "column name")
  expect_error(check_absence_of_zero_counts(input_df, conf_count_col = "conf_count", print_on_fail = "TRUE"), "logical")
})

#####################################################################
test_that("expand to include zeroes creates expected output", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    conf_count = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30)
  )
  
  # complete dataset is unchanged
  output_df = expand_to_include_zero_counts(input_df)
  expect_true(all_equal(input_df, output_df, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  
  # missing rows added back in
  for(ii in 1:nrow(input_df)){
    expected_output = input_df
    expected_output$conf_count[ii] = NA
    
    tmp_input = dplyr::filter(expected_output, !is.na(conf_count))
    
    tmp_output = expand_to_include_zero_counts(tmp_input)
    expect_true(all_equal(expected_output, tmp_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE))
  }
  
  # missing pairs of rows
  for(ii in 1:2){
    for(jj in 4:nrow(input_df)){
      expected_output = input_df
      expected_output$conf_count[ii] = NA
      expected_output$conf_count[jj] = NA
      
      tmp_input = dplyr::filter(expected_output, !is.na(conf_count))

      tmp_output = expand_to_include_zero_counts(tmp_input)
      
      expect_true(
        # either we match the expected output
        all_equal(expected_output, tmp_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE) == TRUE
        # or input = output because we do not need to add any rows
        | all_equal(tmp_input, tmp_output, ignore_row_order = TRUE, ignore_col_order = TRUE, convert = TRUE) == TRUE
      )
    }
  }
})

test_that("expand to include zeroes errors when expected", {
  # arrange
  input_df = data.frame(
    col01 = c("eth","eth","eth","eth","eth","eth","region","region","region","region","region","region","region","region","region","region","region","region"),
    val01 = c("euro","maori","asian","euro","maori","asian","north","north","north","south","south","south","west","west","west","east","east","east"),
    col02 = c("sex","sex","sex","sex","sex","sex","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth","eth"),
    val02 = c("1","1","1","2","2","2","euro","maori","asian","euro","maori","asian","euro","maori","asian","euro","maori","asian"),
    summarised_var = c("x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x"),
    conf_count = c(66,57,60,42,72,75,81,36,57,84,57,30,72,33,75,63,90,30)
  )
  remote_df = dbplyr::tbl_lazy(input_df, con = dbplyr::simulate_mssql())
  alt_df = dplyr::rename(input_df, wrong_col_name = col01)
  
  # act & assert
  expect_error(expand_to_include_zero_counts("input_df"), "dataset")
  expect_error(expand_to_include_zero_counts(remote_df), "local")
  expect_error(expand_to_include_zero_counts(alt_df), "long-thin")
})

#####################################################################
test_that("", {
  
})

test_that("", {
  
})



#' check_confidentialised_results(df, BASE = 3, COUNT_THRESHOLD = 6, SUM_THRESHOLD = 20)
#' explore_output_report(df, output_dir = NA)

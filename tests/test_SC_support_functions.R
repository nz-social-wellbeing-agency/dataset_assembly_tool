###############################################################################
#' Description: Automated tests for summarise and confidentialise functions
#'
#' Input: summary_confidential.R
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
#' 2021-09-08 SA v0
#' #############################################################################

#' Testing the following functions that support summarising and confidentialising
#'
#' cross_product_column_names(..., always = NULL, drop.dupes.within = TRUE, drop.dupes_across = TRUE)
#' has_long_thin_format(df)
#' randomly_round_vector(input_vector, base = 3, seeds = NULL)
#' 
context("summary confidential - support functions")

test_that("cross product outputs", {
  in1 = c("1","2","3")
  in2 = c("4","5")
  out_manual = list(c("1","4"),c("1","5"),c("2","4"),c("2","5"),c("3","4"),c("3","5"))
  out1 = cross_product_column_names(in1,in2)
  out2 = cross_product_column_names(in2,in1)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_manual)
  expect_setequal(out2, out_manual)
})

test_that("always appears in all products", {
  in1 = c("1","2","3")
  in2 = c("4","5")
  in3 = c("6")
  
  out_manual = list(c("1","4","6"),c("1","5","6"),c("2","4","6"),c("2","5","6"),c("3","4","6"),c("3","5","6"))
  out1 = cross_product_column_names(in1,in2, in3)
  out2 = cross_product_column_names(in1,in2, always = in3)
  out1 = lapply(out1, sort)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_manual)
  expect_setequal(out2, out_manual)
  
  out_always = list(c("1","4","5"),c("2","4","5"),c("3","4","5"))
  out1 = cross_product_column_names(in1, always = in2)
  out2 = cross_product_column_names(always = in2, in1)
  out1 = lapply(out1, sort)
  out2 = lapply(out2, sort)
  
  expect_setequal(out1, out_always)
  expect_setequal(out2, out_always)
})

test_that("product removes dupes within", {
  in1 = c("a","b")
  in2 = c("a","c")
  out_dupes_manual = list(c("a","a"),c("a","b"),c("a","c"),c("b","c"))
  out_no_dupes_manual = list(c("a"),c("a","b"),c("a","c"),c("b","c"))
  
  out_dupes = cross_product_column_names(in1, in2, drop.dupes.within = FALSE)
  out_no_dupes = cross_product_column_names(in1, in2, drop.dupes.within = TRUE)
  out_dupes = lapply(out_dupes, sort)
  out_no_dupes = lapply(out_no_dupes, sort)
  
  expect_setequal(out_dupes, out_dupes_manual)
  expect_setequal(out_no_dupes, out_no_dupes_manual)
})

test_that("product removes dupes across", {
  in1 = c("a","b")
  out_dupes_manual = list(c("a","a"),c("a","b"),c("b","a"),c("b","b"))
  out_no_dupes_within_manual = list(c("a"),c("a","b"),c("b","a"),c("b"))
  out_no_dupes_across_manual = list(c("a","a"),c("b","a"),c("b","b"))
  out_no_dupes_both_manual = list(c("a"),c("b","a"),c("b"))
  
  out_dupes = cross_product_column_names(in1, in1, drop.dupes.within = FALSE, drop.dupes.across = FALSE)
  out_no_dupes_within = cross_product_column_names(in1, in1, drop.dupes.within = TRUE, drop.dupes.across = FALSE)
  out_no_dupes_across = cross_product_column_names(in1, in1, drop.dupes.within = FALSE, drop.dupes.across = TRUE)
  out_no_dupes_both = cross_product_column_names(in1, in1, drop.dupes.within = TRUE, drop.dupes.across = TRUE)
  
  expect_setequal(out_dupes, out_dupes_manual)
  expect_setequal(out_no_dupes_within, out_no_dupes_within_manual)
  expect_setequal(out_no_dupes_across, out_no_dupes_across_manual)
  expect_setequal(out_no_dupes_both, out_no_dupes_both_manual)
})


# acceptable formats for long-thin:
# col01 val01 ... col99 val99 summarised_var distinct count sum
# col01 val01 ... col99 val99 summarised_var raw_distinct raw_count raw_sum conf_distinct conf_count conf_sum
test_that("long thin format pases",{
  # arrange
  df1 = data.frame(col01 = 1:5,
                   val01 = 1:5,
                   col02 = 1:5,
                   val02 = 1:5,
                   summarised_var = 1:5,
                   distinct = 1:5,
                   count = 1:5,
                   sum = 1:5)
  df2 = data.frame(col01 = 1:5,
                   val01 = 1:5,
                   col02 = 1:5,
                   val02 = 1:5,
                   summarised_var = 1:5,
                   raw_distinct = 1:5,
                   raw_count = 1:5,
                   raw_sum = 1:5,
                   conf_distinct = 1:5,
                   conf_count = 1:5,
                   conf_sum = 1:5)
  # act
  # assert
  expect_true(has_long_thin_format(df1))
  expect_true(has_long_thin_format(df2))
  expect_true(has_long_thin_format(df1[,3:6]))
  expect_true(has_long_thin_format(df2[,1:7]))
  expect_true(has_long_thin_format(df1[,1:4]))
  expect_true(has_long_thin_format(df2[,10:11]))
  expect_true(has_long_thin_format(df2[,sample(1:11)]))
})

test_that("other formats fail", {
  expect_error(has_long_thin_format(list(1,2,3)), "data\\.frame")
  
  df1 = data.frame(col01 = 1:5,
                   val01 = 1:5,
                   col02 = 1:5,
                   summarised_var = 1:5,
                   count = 1:5)
  expect_false(has_long_thin_format(df1))
  
  df2 = data.frame(col01 = 1:5,
                   val01 = 1:5,
                   not_a_column_name = 1:5,
                   summarised_var = 1:5,
                   count = 1:5)
  expect_false(has_long_thin_format(df2))
})

test_that("random rounding matches base",{
  # arrange
  input = 1:100
  # act
  rr3 = randomly_round_vector(input, base = 3)
  rr4 = randomly_round_vector(input, base = 4)
  rr7 = randomly_round_vector(input, base = 7)
  # assert
  expect_true(all(rr3 %% 3 == 0))
  expect_true(all(rr4 %% 4 == 0))
  expect_true(all(rr7 %% 7 == 0))
})

test_that("random rounding is within base of original", {
  # arrange
  input = 1:100
  # act
  rr3 = randomly_round_vector(input, base = 3)
  rr4 = randomly_round_vector(input, base = 4)
  rr7 = randomly_round_vector(input, base = 7)
  # assert
  expect_true(max(abs(rr3 - input)) < 3)
  expect_true(max(abs(rr4 - input)) < 4)
  expect_true(max(abs(rr7 - input)) < 7)
})

test_that("random rounding is distributed correctly", {
  # arrange
  LENG = 10000
  input = 1:LENG
  
  # act - base 3
  rr3 = randomly_round_vector(input, base = 3)
  diff3 = abs(rr3 - input)
  diff_of_0 = sum(diff3 == 0)
  diff_of_1 = sum(diff3 == 1)
  diff_of_2 = sum(diff3 == 2)
  denom = diff_of_1 + diff_of_2
  
  # assert - base 3
  expect_true(0.30 < diff_of_0 / LENG & diff_of_0 / LENG < 0.36)
  expect_true(0.63 < diff_of_1 / denom & diff_of_1 / denom < 0.69)
  expect_true(0.30 < diff_of_2 / denom & diff_of_2 / denom < 0.36)
  
  # act - base 7
  rr7 = randomly_round_vector(input, base = 7)
  diff7 = abs(rr7 - input)
  # remainders
  remain_of_0 = sum(input %% 7 == 0)
  remain_of_1 = sum(input %% 7 == 1)
  remain_of_2 = sum(input %% 7 == 2)
  remain_of_3 = sum(input %% 7 == 3)
  remain_of_4 = sum(input %% 7 == 4)
  remain_of_5 = sum(input %% 7 == 5)
  remain_of_6 = sum(input %% 7 == 6)
  # difference for each remainder
  remain0_diff0 = sum(input %% 7 == 0 & diff7 == 0)
  remain1_diff1 = sum(input %% 7 == 1 & diff7 == 1)
  remain1_diff6 = sum(input %% 7 == 1 & diff7 == 6)
  remain2_diff2 = sum(input %% 7 == 2 & diff7 == 2)
  remain2_diff5 = sum(input %% 7 == 2 & diff7 == 5)
  remain3_diff3 = sum(input %% 7 == 3 & diff7 == 3)
  remain3_diff4 = sum(input %% 7 == 3 & diff7 == 4)
  remain4_diff3 = sum(input %% 7 == 4 & diff7 == 3)
  remain4_diff4 = sum(input %% 7 == 4 & diff7 == 4)
  remain5_diff2 = sum(input %% 7 == 5 & diff7 == 2)
  remain5_diff5 = sum(input %% 7 == 5 & diff7 == 5)
  remain6_diff1 = sum(input %% 7 == 6 & diff7 == 1)
  remain6_diff6 = sum(input %% 7 == 6 & diff7 == 6)
  # remainders and difference match
  expect_equal(LENG, remain0_diff0 + remain1_diff1 + remain1_diff6 + remain2_diff2 + remain2_diff5 +
                 remain3_diff3 + remain3_diff4 + remain4_diff3 + remain4_diff4 +
                 remain5_diff2 + remain5_diff5 + remain6_diff1 + remain6_diff6)
  # assert - base 7
  expect_true(1.00 <= remain0_diff0 / remain_of_0 & remain0_diff0 / remain_of_0 <= 1.00)
  expect_true(0.80 <= remain1_diff1 / remain_of_1 & remain1_diff1 / remain_of_1 <= 0.91)
  expect_true(0.09 <= remain1_diff6 / remain_of_1 & remain1_diff6 / remain_of_1 <= 0.20)
  expect_true(0.68 <= remain2_diff2 / remain_of_2 & remain2_diff2 / remain_of_2 <= 0.74)
  expect_true(0.26 <= remain2_diff5 / remain_of_2 & remain2_diff5 / remain_of_2 <= 0.32)
  expect_true(0.54 <= remain3_diff3 / remain_of_3 & remain3_diff3 / remain_of_3 <= 0.61)
  expect_true(0.39 <= remain3_diff4 / remain_of_3 & remain3_diff4 / remain_of_3 <= 0.46)
  expect_true(0.54 <= remain4_diff3 / remain_of_4 & remain4_diff3 / remain_of_4 <= 0.61)
  expect_true(0.39 <= remain4_diff4 / remain_of_4 & remain4_diff4 / remain_of_4 <= 0.46)
  expect_true(0.68 <= remain5_diff2 / remain_of_5 & remain5_diff2 / remain_of_5 <= 0.74)
  expect_true(0.26 <= remain5_diff5 / remain_of_5 & remain5_diff5 / remain_of_5 <= 0.32)
  expect_true(0.80 <= remain6_diff1 / remain_of_6 & remain6_diff1 / remain_of_6 <= 0.91)
  expect_true(0.09 <= remain6_diff6 / remain_of_6 & remain6_diff6 / remain_of_6 <= 0.20)
})

test_that("random rounding can be stable", {
  # arrange
  input = 1:10000
  seeds = 123 + 1:10000
  
  # act
  rr3a = randomly_round_vector(input, base = 3, seeds = seeds)
  rr3b = randomly_round_vector(input, base = 3, seeds = seeds)
  rr3c = randomly_round_vector(input, base = 3)
  
  # assert
  expect_true(all(rr3a == rr3b))
  expect_false(all(rr3a == rr3c))
})

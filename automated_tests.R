###############################################################################
#' Description: Automated test suite for R general assembly tool and utility functions
#'
#' Input:
#'
#' Output: Automated test results
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies:
#'
#' Notes:
#'  - Working directory must contain this file, folder of test scripts, and the code files to test
#'  - Sourcing the file runs all scripts (2 minutes), individual tests scripts can be run in order line by line
#'  - Uses code folding by headers (Alt + O to collapse all)
#'
#' Issues:
#'
#' History (reverse order):
#' 2021-10-04 SA output tests added
#' 2020-11-18 SA v2 for release
#' 2020-01-06 SA v1
#' 2019-12-12 SA v0
#' #############################################################################

## setup ------------------------------------------------------------------------------------------

# confirm working directory
ABSOLUTE_PATH_TO_TOOL <- "~/Path/To/Folder/Containing/Assembly/Tool/R/Files"
# confirm project schema
our_schema <- "[DL-MAA20YY-XX]"

setwd(ABSOLUTE_PATH_TO_TOOL)
our_db <- "[IDI_Sandpit]"
our_usercode <- "[IDI_UserCode]"

## test everything --------------------------------------------------------------------------------
#
# For quick runs of all tests when checking that nothing has broken

source("utility_functions.R")
source("table_consistency_checks.R")
source("dbplyr_helper_functions.R")
source("general_assembly_tool_functions.R")
source("general_assembly_tool.R")
source("overview_dataset.R")
source("summary_confidential.R")
source("check_confidentiality.R")
testthat::test_dir("./tests")

## test in sections -------------------------------------------------------------------------------
#
# runs each test file in the recommended order
# (later tests depend on functionality from earlier tests)
#
# Intended for stepping through code, does not run on source

if (FALSE) {
  # test utility_functions.R
  source("utility_functions.R")
  testthat::test_file("./tests/test_UF_independent_functions.R")

  # test table consistency checkers
  source("table_consistency_checks.R")
  testthat::test_file("./tests/test_TCC_base_and_size.R")
  testthat::test_file("./tests/test_TCC_uniques_and_joins.R")
  testthat::test_file("./tests/test_TCC_missings_and_overlaps.R")

  # test dbplyr helpers
  #
  # excludes tests of the functions: create_clustered_index, create_nonclustered_index, write_for_reuse
  # purge_tables_by_prefix, and compress_table
  source("dbplyr_helper_functions.R")
  testthat::test_file("./tests/test_DHF_independent_functions.R")
  testthat::test_file("./tests/test_DHF_connect_read_write.R")
  testthat::test_file("./tests/test_DHF_other_writing.R")
  testthat::test_file("./tests/test_DHF_manipulations.R")

  # test general_assembly_tool_functions.R
  source("general_assembly_tool_functions.R")
  testthat::test_file("./tests/test_GATF_text_handling.R")
  testthat::test_file("./tests/test_GATF_data_handling.R")

  # test general_assembly_tool.R
  source("general_assembly_tool.R")
  testthat::test_file("./tests/test_GAT_function_handling.R")
  testthat::test_file("./tests/test_GAT_integration.R")
  
  # test generating overview of datasets
  source("overview_dataset.R")
  testthat::test_file("./tests/test_OD_filter.R")
  testthat::test_file("./tests/test_OD_report.R")
  
  # test summarising and confidentialising of results
  source("summary_confidential.R")
  testthat::test_file("./tests/test_SC_support_functions.R")
  testthat::test_file("./tests/test_SC_summarise.R")
  testthat::test_file("./tests/test_SC_confidentialise.R")
  
  # test checking of output
  source("check_confidentiality.R")
  testthat::test_file("./tests/test_CC_checks.R")
}

## tidy up ----------------------------------------------------------------------------------------

# remove folder for temporary SQL scripts
unlink("./tests/SQL tmp scripts", recursive = TRUE)

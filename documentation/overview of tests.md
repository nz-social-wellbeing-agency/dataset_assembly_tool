# Overview of tests
2022-08-18

Scope: Covers all existing code shared with the assembly tool. This includes our summarise, confidentialise, and checking tools.

Test types: Tests divide into two broad types (1) tests that are run every time the code is used/run to validate the inputs, and (2) tests that are run during setup to confirm the code all works as expected.


## Data base connection and handling

Tests that occur each time the code is run:
* Required packages are available
* Default connection details have been provided
* Validation of inputs (correct type, format)
* Validation sources exist (database tables, columns, formats)
* The names of SQL objects (databases, schemas, tables) are delimited in square brackets [ ]
* Potential SQL injection is rejected

Tests run to confirm code correctness:
* Validation of inputs passes on valid inputs
* Validation of inputs errors on invalid inputs
* Read, write, edit, create, and delete database objects works
* Consistency of database and local R operations
* Subfunctions perform as expected
* Log files written


## Overview dataset

Tests that occur each time the code is run:
* Validation of inputs (correct type, table in required format, required columns exist)

Tests run to confirm code correctness:
* Validation of inputs passes on valid inputs
* Validation of inputs errors on invalid inputs
* Overview report is produced
* Subfunctions perform as expected


## Dataset Assembly Tool

Tests that occur each time the code is run:
* Validation of control files (format, completeness, contents)
* Validation sources exist (database tables, columns, formats)

Tests run to confirm code correctness:
* Validation of control files passes on valid inputs
* Validation of control files errors on invalid inputs
* Validation that sources exist passes on valid inputs
* Validation that sources exist fails on invalid inputs
* Assembly produces output
* Assembled output is correct
* Subfunctions perform as expected
* Assembly summarise-by calculations are correct
* Full end-to-end test case


## Summarise and confidentialise tools

Tests that occur each time the code is run:
* Validation of inputs (correct type, table in required format, required columns exist)

Tests run to confirm code correctness:
* Validation of inputs passes on valid inputs
* Validation of inputs errors on invalid inputs
* Single summaries produced correctly
* Multiple summaries produced correctly
* Random rounding is correct
* Suppression is correct
* Stable random rounding is consistent
* Subfunctions perform as expected
* Alternative settings perform as expected
* Required format produced
* Robustness of some gaps/missing data
* Full test case


## Confidentiality checking tools

Tests that occur each time the code is run:
* Validation of inputs (correct type, table in required format, required columns exist)
* Specified confidentiality rules are upheld
    * Rounding to required base
    * Rounding is not obviously non-random
    * Suppression is applied
    * Zero counts are handled consistency with 1-5 counts

Tests run to confirm code correctness:
* Validation of inputs passes on valid inputs
* Validation of inputs errors on invalid inputs
* Test case of random rounding is correct
* Test case of suppression is correct
* Test case of overview is correct
* Alternative settings perform as expected
* Overview report is produced


## Consistency checks

Tests that occur each time the code is run:
* Validation of inputs (type, format)

Tests run to confirm code correctness:
* Validation of inputs passes on valid inputs
* Validation of inputs errors on invalid inputs
* Equivalent checks return the same results
* Subfunctions perform as expected
* Alternative settings perform as expected


## Miscellaneous support functions

Tests that occur each time the code is run:
* Validation of inputs (type, format)

Tests run to confirm code correctness:
* Validation of inputs passes on valid inputs
* Validation of inputs errors on invalid inputs
* Functions perform as expected


## File length

To give a sense of how extensive our tests are, the following table lists the total length of the files that provides the tools and functions, and the total length of the files that test each tool or function.

| Tool or function (file names)       | Length of file | Test files prefix | Length of test files |
| ----------------------------------- | -------------- | ----------------- | -------------------- |
| Data base connection and handling   |                |                   |                      |
| (dbplyr_helper_functions.R)         |            740 |          test_DHF |                  470 |
| ----------------------------------- | -------------- | ----------------- | -------------------- |
| Overview dataset                    |                |                   |                      |
| (overview_dataset.R)                |            100 |           test_OD |                  170 |
| ----------------------------------- | -------------- | ----------------- | -------------------- |
| Dataset Assembly Tool               |                |                   |                      |
| (general_assembly_tool.R            |                |         test_GAT  |                      |
| general_assembly_tool_functions.R)  |            810 |         test_GATF |                  780 |
| ----------------------------------- | -------------- | ----------------- | -------------------- |
| Summarise and confidentialise tools |                |                   |                      |
| (summary_confidential.R)            |            510 |           test_SC |                 1210 |
| ----------------------------------- | -------------- | ----------------- | -------------------- |
| Confidentiality checking tools      |                |                   |                      |
| (check_confidentiality.R)           |            540 |           test_CC |                  640 |
| ----------------------------------- | -------------- | ----------------- | -------------------- |
| Consistency checks                  |                |                   |                      |
| (table_consistency_checks.R)        |            320 |          test_TCC |                  450 |
| ----------------------------------- | -------------- | ----------------- | -------------------- |
| Miscellaneous support functions     |                |                   |                      |
| (utility_functions.R)               |            200 |           test_UF |                  160 |
| ----------------------------------- | -------------- | ----------------- | -------------------- |

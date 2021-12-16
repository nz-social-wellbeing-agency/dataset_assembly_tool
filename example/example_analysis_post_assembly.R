# source
source("utility_functions.R")
source("dbplyr_helper_functions.R")
# setup
our_database <- "[IDI_Sandpit]"
our_schema <- "[DL-MAA20XX-YY]"
rectangular_table <- "[tmp_rectangular]"

# connect
db_con <- create_database_connection(database = our_database)
raw_table <- create_access_point(db_con, our_database, our_schema, rectangular_table)

# index to improve performance
create_clustered_index(db_con, our_database, our_schema, rectangular_table, "snz_uid")

# snz_uid list where aged [18,64] with some earnings
ever_earned <- raw_table %>%
  select(identity_column, birth_year, was_employed) %>%
  mutate(age2009 = 2009 - birth_year) %>%
  filter(!is.na(birth_year)) %>%
  filter(
    age2009 >= 18,
    age2009 <= 64
  ) %>%
  filter(!is.na(was_employed)) %>%
  select("identity_column") %>%
  distinct()

# check underlying SQL code
ever_earned %>% show_query()
# save to reduce computation time
ever_earned <- write_for_reuse(db_con, our_database, our_schema, "[tmp_ever_earned]", ever_earned, index_columns = "identity_column")

# tidy assembled table
tidied_table <- raw_table %>%
  # must have ever earned
  semi_join(ever_earned, by = "identity_column") %>%
  # collapse histograms
  collapse_indicator_columns("sex_code=", 1, "sex_code") %>%
  # filter out obvious incomplete records
  filter(
    !is.na(birth_year),
    !is.na(days_in_quarter),
    days_in_quarter > 0
  ) %>%
  # replace missings with nulls
  mutate(
    num_EMS = coalesce(num_EMS, 0),
    total_earnings = coalesce(total_earnings, 0),
    was_employed = coalesce(was_employed, 0)
  )

# size of tidied table
tidied_table %>%
  ungroup() %>%
  summarise(num = n()) %>%
  collect()
# conclusion too large to load into R

# load only 1 person per 100
local_table <- tidied_table %>%
  filter(snz_uid %% 100 == 0) %>%
  collect()

# remove finished with table
delete_table(db_con, our_database, our_schema, "[tmp_ever_earned]")

# close connection
close_database_connection(db_con)

# analyse local table
# or save to disk
# ...
#

######################################################################################
#
# This script creates and saves datasets that will be used internall in functions
#
#######################################################################################
library(tidyverse)

# Create an R object for a crosswalk of county FIPS codes to counyt names
cw <- read_csv('data-raw/puma_counties_cw.csv') %>%
  select(county, cntyname) %>%
  # county FIPS codes have state and county numbers;
  # extract last two numbers, which represent county numbers
  mutate(county = as.integer(str_extract(county, '[0-9]{3}$')),
         # county names do not have a comma between county name and NC
         # and do not include words 'county'; add these back
         cntyname = str_replace(cntyname, " NC$", " County, NC")) %>%
  # remove duplicates
  distinct() %>%
  rename(county_fips = county, county_name = cntyname)

# create dataset of acs variablenames
acs_variables <- read_csv('data-raw/acs_variable_names.csv')

# create lookup table of ACS variables
tables <- load_variables(2017, "acs1", cache = TRUE)
subject <- load_variables(2017, "acs1/subject", cache = TRUE)
acs_lookup <- bind_rows(tables, subject)

# write out dataset
usethis::use_data(cw, acs_variables, acs_lookup,
                  internal = TRUE, overwrite = TRUE)

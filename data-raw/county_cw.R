######################################################################################
#
# This script creates an R object for a crosswalk of county FIPS codes to counyt names
#
#######################################################################################
library(tidyverse)

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

# write out dataset, so it can be used in the packages
usethis::use_data(cw, internal = TRUE)

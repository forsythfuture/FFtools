% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_census.R
\name{ff_single_acs}
\alias{ff_single_acs}
\title{Import ACS data for a single iteration}
\usage{
ff_single_acs(
  geography,
  state,
  county,
  table,
  variables,
  year,
  survey,
  use_table = T
)
}
\description{
This function is used in 'iterate_acs'.
It simply imports data from a single call to the census API through the tidycensus package.
}

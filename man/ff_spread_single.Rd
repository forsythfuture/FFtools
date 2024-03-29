% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning.R
\name{ff_spread_single}
\alias{ff_spread_single}
\title{Intermediate function used in `ff_spread_dual`.  It spreads an individual value,
and then `ff_spread_dual` spreads two individual values using this function, and
combines the two datasets produced from spreading the two individual values.}
\usage{
ff_spread_single(df, key, value_a, value_b, join_cols)
}
\description{
Intermediate function used in `ff_spread_dual`.  It spreads an individual value,
and then `ff_spread_dual` spreads two individual values using this function, and
combines the two datasets produced from spreading the two individual values.
}

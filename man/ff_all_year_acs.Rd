% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_census.R
\name{ff_all_year_acs}
\alias{ff_all_year_acs}
\title{Import ACS data for a single year and topic}
\usage{
ff_all_year_acs(subject, year, table)
}
\arguments{
\item{subject}{table or variable name as a string}

\item{year}{The year to improt data.}

\item{table}{Boolean indicating whether `subject` is a table or variable.
`table = T` indicates that `subject` is a table}
}
\value{
A tidy dataframe with all census data.  Additional columns for the year and geographic
region are added. Geographic regions include US, NC, and all NC counties for the given year.
}
\description{
This function imports ACS data for a single topic and year, and for the following
geographic regions: United States, North Carolina, and all North Carolina counties.
Its use case is to update the indicators.
}
\examples{

employment <- ff_all_year_acs('S2301', 2017, T)

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning.R
\name{ff_acs_filter}
\alias{ff_acs_filter}
\title{Filter rows in ACS data based on column text describing the row}
\usage{
ff_acs_filter(
  df,
  filter_column,
  filter_re,
  subtype_re = NULL,
  recode = F,
  recode_list = NULL
)
}
\arguments{
\item{df}{Dataframe of ACS data}

\item{filter_column}{Column name that contains descriptive text to filter for,
entered as object}

\item{filter_re}{Regular expression used to filter on}

\item{subtype_re}{Regular expression to further extract words from `filter_re` and
place in a new column called `subtype`}

\item{recode}{Boolean indicating whether to recode the newly created `subtype` column}

\item{recode_list}{A named list mapping the recode values}
}
\value{
A dataframe only containing the filtered rows; and an additiona lcolumn called `subtype` if
`recode = T`.
}
\description{
With this function, users can imput a regular expression and column with descriptive
text, and the dataset will be filtered based on the regualr expression.
}
\details{
Users can also extract a section of the regular expression, and place this phrase
into a different  column titled `subtype`.  Finally users can recode the newly
created `subtype` column by enting a named list of values.
}
\examples{
age_bins <- list(`16 to 26 year old` = "16 to 25",
                 `75 and over` = "over 75")

ff_acs_filter(employment, description, "AGE -", "[0-9].*",

}

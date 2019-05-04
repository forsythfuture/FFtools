#' Filter rows in ACS data based on column text describing the row
#'
#' With this function, users can imput a regular expression and column with descriptive
#' text, and the dataset will be filtered based on the regualr expression.
#'
#' Users can also extract a section of the regular expression, and place this phrase
#' into a different  column titled `subtype`.  Finally users can recode the newly
#' created `subtype` column by enting a named list of values.
#'
#' @param df Dataframe of ACS data
#' @param filter_column Column name that contains descriptive text to filter for,
#' entered as object
#' @param filter_re Regular expression used to filter on
#' @param subtype_re Regular expression to further extract words from `filter_re` and
#' place in a new column called `subtype`
#' @param recode Boolean indicating whether to recode the newly created `subtype` column
#' @param recode_list A named list mapping the recode values
#' @return A dataframe only containing the filtered rows; and an additiona lcolumn called `subtype` if
#' `recode = T`.
#' @examples
#' age_bins <- list(`16 to 26 year old` = "16 to 25",
#'                  `75 and over` = "over 75")
#'
#' ff_acs_filter(employment, description, "AGE -", "[0-9].*",
#                recode = T, recode_list = age_bins)
#'
#' @export
#' @importFrom magrittr "%>%"
ff_acs_filter <- function(df, filter_column, filter_re, subtype_re = NULL,
                          recode = F, recode_list = NULL) {

  filter_column <- rlang::enquo(filter_column)

  df <- df %>%
    # filter based on the filtering regular expression
    dplyr::filter(stringr::str_detect(!! filter_column, filter_re))

  # if there is a subtype regular expression, extract this phrase from the filter columns
  if (!is.null(subtype_re)) {

    df <- df %>%
      dplyr::mutate(subtype = stringr::str_extract(!! filter_column, subtype_re))

  }

  # recode variables if required
  if (recode == T) {

    df <- df %>%
      dplyr::mutate(subtype = dplyr::recode(rlang::.data$subtype, !!! recode_list))
  }

  return(df)

}

#' Intermediate function used in `ff_spread_dual`.  It spreads an individual value,
#' and then `ff_spread_dual` spreads two individual values using this function, and
#' combines the two datasets produced from spreading the two individual values.
ff_spread_single <- function(df, key, value_a, value_b, join_cols) {

  df <- df %>%
    dplyr::select(join_cols, key, value_a) %>%
    tidyr::spread(key = key, value = value_a)

  # rename columns so that when the datasets are merged we know
  # what the column names mean
  colnames(df) <- c(join_cols,
                    stringr::str_c(value_a, '_a'),
                    stringr::str_c(value_a, '_b'))

  return(df)

}

#' Spread columns when there are two value columns that need to be retained.
#'
#' The `spread` function only takes one value column.  But, we often need to spread a dataset
#' into wide-form, but with two value columns.  A example is when we want to spread a dataset,
#' but keep the estimate and standard error columns.  This function allows users to spread
#' a dataset and keep two value columns.

#' @param df Dataframe of ACS data
#' @param key Key column in dataframe as string (the column with the information we want to spread on)
#' @param value_a First value column we want to keep as string
#' @param value_b Second value column we want to keep as string
#' @param join_cols Each value column is spread individually, and then both new spread datasets are joined.
#' `join_cols` signifies which column to join the to datasets on.
#' @return A dataframe with seperate columns for each of the values spread on, and value_a and value_b.
#' The final dataframe will contain all the join columns and value columns totaling the number of items
#' to spread on times two (two different values that are kept when spreading).
#' @examples
#' ff_spread_dual(total, 'description', 'estimate', 'se', c('geo_description', 'year'))
#'
#' @export
#' @importFrom magrittr "%>%"
ff_spread_dual <- function(df, key, value_a, value_b, join_cols) {

  spread_a <- ff_spread_single(df, key, value_a, value_b, join_cols)
  spread_b <- ff_spread_single(df, key, value_b, value_a, join_cols)

  spread_full <- dplyr::full_join(spread_a, spread_b, by = join_cols)

  return(spread_full)

}


#' Filter for race / ethnicity rows in ACS data
#'
#' This function filters for the following ethnicities in ACS data:
#'      White Alone, Not Hispanic or Latino; Black or African American Alone; Hispanic or Latino
#'
#' It also cleans the race / ethnicity name to match FF standards.
#'
#' @param df Dataframe of ACS data
#' @param ethnicity_column Column name that contains race / ethnicity phrase
#' @return The dataframe inputted into the function, but only the rows with race / ethnicity information
#'         for White Alone, Not Hispanic or Latino; Black or African American Alone; and Hispanic or Latino remain
#' @examples
#' ff_acs_ethnicity(df, description)
#'
#' @export
#' @importFrom magrittr "%>%"
ff_acs_ethnicity <- function(df, ethnicity_column) {

  # keep these ethnicities; these are how the ethnicities are worded in downloaded ACS files
  keep_ethnicities <- c('White alone, not Hispanic or Latino', 'Black or African American',
                        'Hispanic or Latino origin')

  # using ACS ethnicites, create regular expression that will search for any of the ethnicities
  # the '|' signifies or
  re_ethnicities <- paste(keep_ethnicities, collapse = '|')

  # convert ethnicity column object to eunquo object
  # this allows us to use the object as a column name within dplyr
  ethnicity_column <- rlang::enquo(ethnicity_column)

  df <- df %>%
    # filter for rows that contain ethnicity information;
    # !! signifies that the column name is stored in an object, and is not a column in the dataframe
    dplyr::filter(stringr::str_detect(!! ethnicity_column, re_ethnicities)) %>%
    # create new column that is only the name of the ethnicity
    dplyr::mutate(subtype = stringr::str_extract(!! ethnicity_column, re_ethnicities)) %>%
    # convert ethnicity names to Forsyth Futures conventions
    dplyr::mutate(subtype = ifelse(.$subtype == 'Black or African American', 'African American',
                                ifelse(.$subtype == 'Hispanic or Latino origin', 'Hispanic/Latino',
                                   ifelse(.$subtype == 'White alone, not Hispanic or Latino', 'White, non-Hispanic', 'Not sure'))),
                  type = 'Ethnicity')

  return(df)

}

#' Filter for age rows in ACS data and rebin ages if needed
#'
#' This function filters ACS data for rows that include ages.  It also has optional
#' parameters to rebin age groups into custom age bins.
#'
#' @param df Dataframe of ACS data
#' @param age_column Column name that contains age information
#' @param recode Boolean value of whether to recode age bins
#' @param recode_list A named list where name is old age bin and value is a string for the new age bin
#'
#' @return Dataframe that only contains the age rows
#' @examples
#' age_bins <- list(`16 to 26 year old` = "16 to 25",
#'                  `75 and over` = "over 75")
#' ff_acs_age(df, description, recode = T, recode_list = age_bins)
#'
#' @export
#' @importFrom magrittr "%>%"
ff_acs_age <- function(df, age_column, recode = F, recode_list = NULL) {

  age_column <- rlang::enquo(age_column)

  df <- df %>%
    # only keep age variable
    dplyr::filter(stringr::str_detect(!! age_column, 'AGE')) %>%
    # create new variable that is only the age
    dplyr::mutate(subtype = stringr::str_extract(!! age_column, 'AGE.*'),
                  subtype = stringr::str_extract(subtype, '[0-9].*'),
                  type = 'Age')

  # recode age variables if required
  if (recode == T) {

    df <- df %>%
      dplyr::mutate(subtype = dplyr::recode(rlang::.data$subtype, !!! recode_list))
  }

  return(df)

}

#' Filter ACS data for specific variables
#'
#' ACS variables are three digit numbers shown as the last three digits in the 'variables' column.
#' The function filters a dataframe of ACS data for specified variables.
#'
#' @param df Dataframe of ACS data
#' @param variables Vector of strings signifying variable numbers
#' @return Same dataframe as input, but only the rows with the needed variables are included.
#' @examples
#' ff_acs_keep_vars(df, c('100', '101'))
#'
#' @export
#' @importFrom magrittr "%>%"
ff_acs_keep_vars <- function(df, variables) {

  # check vector of variables and ensure it is a character vector
  # if not, through error message
  if (!is.character(variables))
    stop("The vector of variable names must be characters.To create characters, surround numbers in ''.")

  df %>%
    # extract last three digits, which are the variables, and place in new column
    dplyr::mutate(variable_code = stringr::str_extract(.$variable, "[0-9]{3}$")) %>%
    # filter this column for the specific variables
    dplyr::filter(variable_code %in% variables) %>%
    # remove column that is only the variable code
    dplyr::select(-variable_code)

}

#' Match county names to county FIPS codes
#'
#' This function performs a cross-walk between county names and county FIPS codes.
#' It takes NC county FIPS codes and input and outputs the county name.
#'
#' @param county_fips_codes NC county FIPS codes.
#' @return The county name as a string corresponding to the county FIPS code.
#' @examples
#' df <- data.frame(county_fips_code = c(1, 3, 67),
#'                  estimate = c(121, 156, 190))
#'
#' ff_county_fips(df$county_fips_code)
#'
#' # Directly adding county names as a column
#' dplyr::mutate(df, county_name = ff_county_fips(df$county_fips_code))
#'
#' @export
#' @importFrom magrittr "%>%"
ff_county_fips <- function(county_fips_codes) {

  # turn vector of FIPS codes into dataframe
  # required so we can merge codes with names
  fips_df <- data.frame(county_fips = county_fips_codes) %>%
    # merge fips code dataframe with county names / FIPS crosswalk
    dplyr::left_join(cw, by = 'county_fips')

  # return as vector
  return(fips_df$county_name)

}

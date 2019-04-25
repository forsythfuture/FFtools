#' Filter for race / ethnicity rows in ACS data
#'
#'   This function filters for the following ethnicities in ACS data:
#'      White Alone, Not Hispanic or Latino; Black or African American Alone; Hispanic or Latino
#'
#'   It also cleans the race / ethnicity name to match FF standards.
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
    dplyr::mutate(ethnicity = stringr::str_extract(!! ethnicity_column, re_ethnicities)) %>%
    # convert ethnicity names to Forsyth Futures conventions
    dplyr::mutate(ethnicity = ifelse(.$ethnicity == 'Black or African American', 'African American',
                                ifelse(.$ethnicity == 'Hispanic or Latino origin', 'Hispanic/Latino',
                                   ifelse(.$ethnicity == 'White alone, not Hispanic or Latino', 'White, non-Hispanic', 'Not sure'))))

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
#' @example
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

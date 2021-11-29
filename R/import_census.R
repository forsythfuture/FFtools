#' Import ACS data for a single iteration
#'
#' This function is used in 'iterate_acs'.
#' It simply imports data from a single call to the census API through the tidycensus package.
ff_single_acs <- function(geography, state, county, table, variables, year,
                        survey, use_table = T) {

  if (use_table == T) {

    tidycensus::get_acs(geography = geography,
                        state = if (is.na(state)) NULL else state,
                        county = if (is.na(county)) NULL else county,
                        table = table,
                        year = year,
                        survey = survey) %>%
      dplyr::mutate(year = year,
                    geography = geography)

  } else if (use_table == F) {

    tidycensus::get_acs(geography = geography,
                        state = if (is.na(state)) NULL else state,
                        county = if (is.na(county)) NULL else county,
                        variables = variables,
                        year = year,
                        survey = survey) %>%
      dplyr::mutate(year = year,
                    geography = geography)

  } else {

    stop('use_table must be either TRUE or FALSE')

  }

}

#' Iteratively import ACS data
#'
#' This function makes repeated calls to the Census API through the tidycensus package.
#' It allows users to import multiple years and geographic areas with one function call.
#'
#' @param parameters_list A named list of parameters to feed into the get_acs function from
#'   tidycensus.  The name of each item should correspond to the parameter name in get_acs.
#' @return A tidy dataframe with all census data.  Additional columns for the yar and geographic
#' region are added.
#' @examples
#' parameters <- list(geography = c('us', 'state', 'county', 'tract'),
#'                    state = c(NA, rep('NC', 3)),
#'                    county = c(rep(NA, 3), 'Forsyth'),
#'                    table = c(rep('S2301', 3), NA),
#'                    variables = c(rep(NA, 3), 'S2301_C03_001'),
#'                    survey = c(rep('acs1', 3), 'acs5'),
#'                    year = rep(current_year, 4),
#'                    use_table = c(rep(T, 3), F))
#'
#' employment <- ff_iterate_acs(parameters)
#'
#' @export
#' @importFrom magrittr "%>%"
ff_iterate_acs <- function(parameters_list) {

  # iterate through acs api calls
  acs <- purrr::pmap(parameters_list, ff_single_acs) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(acs_lookup, by = c('variable' = 'name')) %>%
    # calculate standard error from 95% confidence interval
    dplyr::mutate(se = moe / 1.96) %>%
    # rename columns to match standards
    dplyr::rename(geo_description = NAME, description = label) %>%
    dplyr::select(-GEOID, -variable, -moe) %>%
    # change county name labels from "County, North Carolina" to "County, NC"
    dplyr::mutate(geo_description = str_replace(geo_description, "County, North Carolina", "County, NC"))

  return(acs)

}

#' Import ACS data for a single year and topic
#'
#' This function imports ACS data for a single topic and year, and for the following
#' geographic regions: United States, North Carolina, and all North Carolina counties.
#' Its use case is to update the indicators.
#'
#' @param subject table or variable name as a string
#' @param year The year to improt data.
#' @param table Boolean indicating whether `subject` is a table or variable.
#' `table = T` indicates that `subject` is a table
#' @return A tidy dataframe with all census data.  Additional columns for the year and geographic
#' region are added. Geographic regions include US, NC, and all NC counties for the given year.
#' @examples
#'
#' employment <- ff_all_year_acs('S2301', 2017, T)
#'
#' @export
#' @importFrom magrittr "%>%"
ff_all_year_acs <- function(subject, year, table) {

  parameters <- list(geography = c('us', 'state', 'county'),
                     state = c(NA, rep('NC', 2)),
                     county = c(rep(NA, 3)),
                     table = rep(if (table == T) subject else NA, 3),
                     variables = rep(if (table == F) subject else NA, 3),
                     survey = rep('acs1', 3),
                     year = rep(year, 3),
                     use_table = rep(table, 3))

  df <- ff_iterate_acs(parameters)

  return(df)

}

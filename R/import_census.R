#' Import ACS data for a single iteration
#'
#' This function is used in 'iterate_acs'.
#' It simply imports data from a single call to the census API through the tidycensus package.
single_acs <- function(geography, state, county, table, variables, year,
                        survey, use_table = T) {

  if (use_table == T) {

    tidycensus::get_acs(geography = geography,
                        state = if (is.na(state)) NULL else state,
                        county = if (is.na(county)) NULL else county,
                        table = table,
                        year = year,
                        survey = survey)

  } else if (use_table == F) {

    tidycensus::get_acs(geography = geography,
                        state = if (is.na(state)) NULL else state,
                        county = if (is.na(county)) NULL else county,
                        variables = variables,
                        year = year,
                        survey = survey)

  } else {

    stop('use_table must be either TRUE or FALSE')

  }

}

#' Iteartively import ACS data
#'
#' This function makes repeated calls to the Census API through the tidycensus package.
#' It allows users to import multiple years and geographic areas with one function call.
#'
#' @param parameters_list A named list of parameters to feed into the get_acs function from
#'   tidycensus.  The name of each item should correspond to the parameter name in get_acs.
#' @param variable_names A dataframe that contains the variable names.  Default is NULL.
#' @return A tidy dataframe with all census data.
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
#' variable_descriptions <- load_variables(2017, "acs1", cache = TRUE)
#' employment <- iterate_acs(parameters, variable_descriptions)
#'
#' @export
#' @importFrom magrittr "%>%"
iterate_acs <- function(parameters_list, variable_names = NULL) {

  # iterate through acs api calls
  acs <- purr::pmap(parameters_list, single_acs) %>%
    dplyr::bind_rows()

  # if a dataframe of variable names is supplied, add them to the dataset
  if (!is.null(variable_names)) {

    acs <- acs %>%
      dplyr::left_join(variable_names, by = c('variable' = 'name'))

  }

  return(acs)

}

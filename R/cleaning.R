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

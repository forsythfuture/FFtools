#' Inflation Adjustments
#'
#' This function converts dollar amounts to a given year's dollar amount.
#' Reference for calculations:
#' US Census Bureau, A Compass for Understanding and Using ACS Data, October 2008, A-22
#'
#' @param data_frame Name of dataframe that contains dollar amounts
#' @param wages_col String representing the column name containing dollar amounts
#' @param se_col String representing the column name containing dollar amounts.
#'     Value will be blank if `errors = FALSE`.
#' @param year_col String representing the column name containing years
#' @param year_adjust  Integer signifying the year to adjust dollar amounts to
#' @param errors Boolean (TRUE / FALSE) signifying whether dataset contains standard errors that also need to be adjusted
#' @return The same data frame that was given as input, but with `wages_col` now representing inflation adjusted wages.
#'     If `errors = TRUE`, the standard error column will also signify the inflation adjusted standard errors.
#' @examples
#' df <- data.frame(year = seq(2008, 2017, 1),
#'                  wages = round( rnorm(10, 30000, 10000), 0),
#'                  se = round( rnorm(10, 1000, 100), 0))
#'
#' ff_inflation_adjust(df, wages_col = 'wages', se_col = 'se', year_col = 'year',
#'                     year_adjust = 2017, errors = TRUE)
#'
#' @export
#' @importFrom magrittr "%>%"
ff_inflation_adjust <- function(data_frame, wages_col, se_col, year_col, year_adjust, errors = FALSE) {

  # import CPI All items index data
  monthly_cpi <- utils::read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                                    skip = 53, header = TRUE)

  # extract year and place in its own column
  # have the column be the same name as the year column in the input dataframe,
  # so we can easily merge on year
  monthly_cpi[[year_col]] <- lubridate::year(monthly_cpi$DATE)

  # calculate mean CPI for the year
  yearly_cpi <- monthly_cpi %>%
    dplyr::group_by_at(dplyr::vars(year_col)) %>%
    dplyr::summarize(cpi = mean(VALUE))

  # calculate inflation rate compared to adjustment year
  yearly_cpi$adj_factor <- yearly_cpi$cpi[yearly_cpi[[year_col]] == year_adjust]/yearly_cpi$cpi

  # combine inflation adjusted wages to wages dataset
  data_frame <- dplyr::left_join(data_frame, yearly_cpi, by = year_col) %>%
    # adjust income in the given year for inflation since the base year
    # multiply the wage amount in the current year by the adjustment factor
    dplyr::mutate_at(dplyr::vars(wages_col),
                     dplyr::funs(round(. * adj_factor, 0)))

  # adjust standard error if needed
  if (errors == TRUE) {

    data_frame[[se_col]] <- data_frame[[se_col]] - data_frame[['adj_factor']]
    data_frame[[se_col]] <- round(data_frame[[se_col]], 0)

  }

  data_frame <- data_frame %>%
    dplyr::select(-cpi, -adj_factor)

  return(data_frame)

}

#' Standard error for aggregated count data
#'
#' This function calculates the standard error for aggregated count data and summed data. For example,
#' this can be used to calculate the standard error for the total population of Forsyth and Guilford
#' counties.
#'
#' Reference for calculations:
#' US Census Bureau, A Compass for Understanding and Using ACS Data, October 2008, A-14
#'
#' @param se_vec A numeric vector of values representing standard errors of count data
#' @return A numeric value representing the standard error of the aggregate count data.
#' @examples
#' df <- data.frame(population = round( rnorm(n = 10, mean = 100000, sd = 30000), 0),
#'                  se = round( rnorm(n = 10, mean = 4000, sd = 1500), 0))
#'
#' # calculate the aggregate population
#' sum(df$population)
#'
#' # calculate the aggregate standard error
#' ff_se_count(df$se)
#'
#' @export
#' @importFrom magrittr "%>%"
ff_se_count <- function(se_vec) {

  # square each standard error
  se_vec^2 %>%
    # sum the squared standard errors
    sum(.) %>%
    # take the square root of the sums
    sqrt(.)

}

#' Standard error for derived proportions
#'
#' This function calculates the standard error for derived proportions. The numerator must be a subset of
#' the denominator. For example, the numerator might be the population of african-american males with a
#' college degree, while the denominator is the population of african american males. Use `ff_se_ratio`
#' if the numerator is not a subset of the denominator.
#'
#' The function will fail if the value under the square root is negative. In such a case, users will see
#' the following message:
#' `Warning message: In sqrt(...) : NaNs produced.`
#'
#' If this occurs, use `ff_se_ratio`.
#'
#' Reference for calculations:
#' US Census Bureau, A Compass for Understanding and Using ACS Data, October 2008, A-14
#'
#' @param estimate_num The numerator for the estimated proportion
#' @param estimate_den The denominator for the estimated proprotion
#' @param se_num Standard error for the numerator of the derived proportions.
#' @param se_den Standard error for the denominator of the derived proportions
#' @return The standard error of the derived proportions
#' @examples
#'df <- data.frame(estimate_numerator = rnorm(n = 10, mean = 2000, sd = 500),
#'                 estimate_denominator = rnorm(n = 10, mean = 10000, sd = 2000),
#'                 se_numerator = rnorm(n = 10, mean = 200, sd = 50),
#'                 se_denominator = rnorm(n = 10, mean = 100, sd = 10))
#'
#' # calculate standard errors of proportions for each observation
#' ff_se_prop(df$estimate_numerator, df$estimate_denominator,
#'            df$se_numerator, df$se_denominator)
#'
#' # add columns to dataframe showing proportions and standard errors of proportions
#' dplyr::mutate(df,
#'               proportion = estimate_numerator / estimate_denominator,
#'               se_proportion = ff_se_prop(estimate_numerator, estimate_denominator,
#'                                          se_numerator, se_denominator))
#'
#' @export
ff_se_prop <- function(estimate_num, estimate_den, se_num, se_den) {

  # calculate proportion for use in formula
  prop <- estimate_num / estimate_den

  # calculate the standard error
  ( sqrt(se_num^2 - (prop^2 * se_den^2)) ) / estimate_den

}

#' Standard error for derived ratios
#'
#' This function calculates the standard error for derived ratios. An example of a ratio is the median
#' income of males divided by the median income of females. If the data is a proportion instead of a
#' ratio, use `ff_se_prop`.
#'
#' Reference for calculations:
#' US Census Bureau, A Compass for Understanding and Using ACS Data, October 2008, A-15
#'
#' @param estimate_num The numerator for the estimated proportion
#' @param estimate_den The denominator for the estimated proprotion
#' @param se_num Standard error for the numerator of the derived proportions.
#' @param se_den Standard error for the denominator of the derived proportions
#' @return The standard error of the derived ratios.
#' @examples
#'df <- data.frame(estimate_numerator = rnorm(n = 10, mean = 2000, sd = 500),
#'                 estimate_denominator = rnorm(n = 10, mean = 10000, sd = 2000),
#'                 se_numerator = rnorm(n = 10, mean = 200, sd = 50),
#'                 se_denominator = rnorm(n = 10, mean = 100, sd = 10))
#'
#' # calculate standard errors of proportions for each observation
#' ff_se_ratio(df$estimate_numerator, df$estimate_denominator,
#'            df$se_numerator, df$se_denominator)
#'
#' # add columns to dataframe showing proportions and standard errors of proportions
#' dplyr::mutate(df,
#'               ratio = estimate_numerator / estimate_denominator,
#'               se_ratio = ff_se_ratio(estimate_numerator, estimate_denominator,
#'                                      se_numerator, se_denominator))
#'
#' @export
ff_se_ratio <- function(estimate_num, estimate_den, se_num, se_den) {

  # calculate proportion for use in formula
  ratio <- estimate_num / estimate_den

  # calculate the standard error
  (sqrt(se_num^2 + (ratio^2 * se_den^2))) / estimate_den

}

#' Standard error for the product of two estimates
#'
#' This function calculates the standard error for the product of two estimates. An example is if users want
#' to derive the number of people working by multiplying the total population by the percentage of the
#' population that is working.
#'
#' Reference for calculations:
#' US Census Bureau, A Compass for Understanding and Using ACS Data, October 2008, A-16
#'
#' @param estimate_first Point estimate of the first value.
#' @param se_first Standard error for the first estimate.
#' @param estimate_second Point estimate of the second value.
#' @param se_second Standard error for the second estimate
#' @return The standard error of the derived products.
#' @examples
#'df <- data.frame(pop = rnorm(n = 10, mean = 100000, sd = 20000),
#'                 se_pop = rnorm(n = 10, mean = 1000, sd = 200),
#'                 perc_working = runif(n = 10, min = .6, max = .9),
#'                 perc_working_se = runif(n = 10, min = .05, max = .2))
#'
#' # calculate standard errors of the products for each observation
#' ff_se_ratio(df$pop, df$se_pop,
#'            df$perc_working, df$perc_working_se)
#'
#' # add columns to dataframe showing products and standard errors of products
#' dplyr::mutate(df,
#'               total_working = pop / perc_working,
#'               se_total_working = ff_se_prod(pop, se_pop,
#'                                             perc_working, perc_working_se))
#'
#' @export
ff_se_prod <- function(estimate_first, se_first, estimate_second, se_second) {

  sqrt( (estimate_first^2*se_second^2) + (estimate_second^2*se_first^2) )

}

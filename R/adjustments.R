#' Inflation Adjustments
#'
#' This function converts dollar amounts to a given year's dollar amount.
#' Reference for calculations:
#'     US Census Bureau, A Compass for Understanding and Using ACS Data, October 2008, A-22
#'
#' @param data_frame Name of dataframe that contains dollar amounts
#' @param year_adjust  Integer signifying the year to adjust dollar amounts to
#' @param year_col String representing the column name containing years
#' @param errors Boolean (TRUE / FALSE) signifying whether dataset contains standard errors that also need to be adjusted
#' @param wages_col String representing the column name containing dollar amounts
#' @param se_col String representing the column name containing dollar amounts.
#'     Value will be blank if `errors = FALSE`.
#' @return The same data frame that was given as input, but with `wages_col` now representing inflation adjusted wages.
#'     If `errors = TRUE`, the standard error column will also signify the inflation adjusted standard errors.
#' @examples
#' df <- data.frame(year = seq(2008, 2017, 1),
#'                  wages = round( rnorm(10, 30000, 10000), 0),
#'                  se = round( rnorm(10, 1000, 100), 0))
#' @export
#' @importFrom magrittr "%>%"
ff_inflation_adjust <- function(data_frame, wages_col, se_col, year_adjust, error = FALSE) {

  library(xts)
  library(lubridate)

  # import CPI All items index data
  monthly_cpi <- read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                            skip = 53, header = TRUE)

  # extract year and place in its own column
  monthly_cpi$year <- year(monthly_cpi$DATE)

  # calculate mean CPI for the year
  yearly_cpi <- monthly_cpi %>%
    group_by(year) %>%
    summarize(cpi = mean(VALUE))

  # calculate inflation rate compared to adjustment year
  yearly_cpi$adj_factor <- yearly_cpi$cpi[yearly_cpi$year == year_adjust]/yearly_cpi$cpi

  # combine inflation adjusted wages to wages dataset
  data_frame <- left_join(data_frame, yearly_cpi, by = c(!!year_col = 'year')) %>%
    # adjust income in the given year for inflation since the base year
    # multiply the wage amount in the current year by the adjustment factor
    mutate_at(vars(!!wage_col),
              funs(round(!!wages_col * adj_factor, 0)))

  # adjust standard error is needed
  if (error == TRUE) {

    data_frame[[se_col]] <- data_frame[[se_col]] - data_frame[['adj_factor']]
    data_frame[[se_col]] <- round(data_frame[[se_col]], 0)

  }

  data_frame <- data_frame %>%
    select(-cpi, -adj_factor)

  return(data_frame)
}

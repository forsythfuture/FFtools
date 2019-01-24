# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test packages:             'Ctrl + Shift + T'
#
# df <- data.frame(year = c(2016, 2016, 2017, 2017),
#                  geo_description = c('Forsyth County, NC', 'Guilford County, NC',
#                                      'Forsyth County, NC', 'Guilford County, NC'),
#                  estimate = c(.66, .63, .88, .48),
#                  success = c(10, 12, 15, 19),
#                  trials = c(15, 19, 17, 39))
#
# read.csv('https://raw.githubusercontent.com/forsythfuture/indicators/master/shiny_datasets/infant_mortality.csv') %>%
#   ff_sigtest(., 'estimate', 'se', test = 'chi-square', success = 'success', trials = 'trials',
#              var_names = c('year', 'geo_description', 'subtype'),
#              pretty_print = TRUE)

data_frame <- data.frame(year = seq(2008, 2017, 1),
                 wages = round( rnorm(10, 30000, 10000), 0),
                 se = round( rnorm(10, 1000, 100), 0))
adj <- function(dataf, wage_col) {

  dataf %>%
    mutate_at(vars(!!wage_col),
              funs(. * 10))
}

adj(df, 'wages')



library(xts)
library(lubridate)

ff_inflation_adjust <- function(data_frame, wages_col, se_col, year_adjust, error = FALSE) {

  # import CPI All items index data
  monthly_cpi <- read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                            skip = 53, header = TRUE)

  # extract year and place in its own column
  # have the column be the same name as the year column in the input dataframe,
  # so we can easily merge on year
  monthly_cpi[[year_col]] <- year(monthly_cpi$DATE)

  # calculate mean CPI for the year
  yearly_cpi <- monthly_cpi %>%
    group_by_at(vars(year_col)) %>%
    summarize(cpi = mean(VALUE))

  # calculate inflation rate compared to adjustment year
  yearly_cpi$adj_factor <- yearly_cpi$cpi[yearly_cpi[[year_col]] == year_adjust]/yearly_cpi$cpi

  # combine inflation adjusted wages to wages dataset
  data_frame <- left_join(data_frame, yearly_cpi, by = year_col) %>%
    # adjust income in the given year for inflation since the base year
    # multiply the wage amount in the current year by the adjustment factor
    mutate_at(vars(wage_col),
              funs(round(wages_col * adj_factor, 0)))

  # adjust standard error is needed
  if (error == TRUE) {

    data_frame[[se_col]] <- data_frame[[se_col]] - data_frame[['adj_factor']]
    data_frame[[se_col]] <- round(data_frame[[se_col]], 0)

  }

  data_frame <- data_frame %>%
    select(-cpi, -adj_factor)

  return(data_frame)

}


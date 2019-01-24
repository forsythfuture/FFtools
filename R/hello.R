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


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
hello <- function() {
  print("Hello, world!")
}

# df <- data.frame(year = c(2016, 2017),
#                  geo_description = c('Forsyth County, NC', 'Guilford County, NC'),
#                  estimate = c(1,2),
#                  se = c(.2, .3),
#                  success = c(10, 12),
#                  trials = c(15, 19))
#
# ff_sigtest(data_frame = df, estimate = 'estimate', se = 'se',
#            test = 'zscore', var_names = c('year', 'geo_description')) %>%
#   # Pipe significance testing matrix into function creating kable table
#   ff_sigtest_kable(sigtest_matrix = ., test = 'zscore', table_name = 'Example table')


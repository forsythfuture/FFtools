# Package of common functions
The `FFtools` package contains common Forsyth Futures functions.

## Installation
1. Install the `devtools` package, if not already installed: `install.packages('devtools')`.
2. Install the `FFtools` package directly from GitHub:  `devtools::install_github('forsythfuture/FFtools')`

## Functions

### Data Analysis

#### Matrix of significance tests

`ff_sigtest`

This function returns a square symetrical matrix of all significance tests for all combinations of values. It calculates the p-value from either the z test statistic or Chi-Square test statistic. A two-sided significance test with a 95% confidence level is conducted and the null hypothesis is that there is no difference between the two parameters. The function returns a matrix with the length and width equal the number of rows in the data frame.

The z-score formula comes from:
U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18.

The z-scores are then converted to p-values using the R function for generating cumulative PDFs: `pnorm(z_score, lower.tail=FALSE)*2`.

The Chi-Square test of proportions uses `prop.test` and extracts the p-values from this function's results.

**Parameters**
* `data_frame` A dataframe containing estimates and either standard errors for z-score test or successes and trials for a Chi-Square test.
* `estimate` The column name of number to conduct significance tests on.
* `se` Column name of the standard error of the estimate. Required if test a z-score test
* `test` The significances test to conduct. Either "zscore" or "chi-square". Defaults to 'zscore'.
* `success` Column name of the number of successful trials. Required for Chi-Square test.
* `trials` COlumn name of the total number of trials. Required for Chi-Square test.
* `var_names` A character vector of variables that can be combined to create distinct names for each row and column.
* `pretty_print` Boolean (TRUE / FALSE) indicating whether to return the table as a Kable HTML table that bolds statistically significant finding and creates other stylistic changes. Default is FALSE.
* `table_name` (optional) Character string to use as the name of the Kable table. Only used if `pretty_print` is TRUE.

**Example**

```r
df <- data.frame(year = c(2016, 2017),
                geo_description = c('Forsyth County, NC', 'Guilford County, NC'),
                estimate = c(1,2),
                se = c(.2, .3),
                success = c(10, 12),
                trials = c(15, 19))

# Z score test
ff_sigtest(data_frame = df, estimate = 'estimate', se = 'se',
           test = 'zscore', var_names = c('year', 'geo_description'))

# Chi-Square test
ff_sigtest(data_frame = df, estimate = 'estimate', success = 'success', trials = 'trials',
           test = 'chi-square', var_names = c('year', 'geo_description'))
```

#### Matrix of estimates and confidence intervals

`ff_estimates_ci`

This function returns a square symetrical matrix of all differences between all combinations of rows, along with the 95 percent confidence interval of the difference. For binomial (categorical) datasets, the difference is the percentile difference.

**Parameters:**
* `data_frame` A dataframe containing estimates and either standard errors for continuous data or successes and trials for binomial data.
* `estimate` An integer or float containing the number to compare differences. Required for continuous format.
* `se` Standard error of the estimate. Required for continuous format.
* `format` Type of data; either 'continuous' or 'binomial'. If continuous, the `estimate` columns is used to generate differences. If binomial, the `success` and `trials` columns are used. Default is continuous.
* `success` The number of successful trials. Required for binomial format.
* `trials` The total number of trials. Required for binomial format.
* `var_names` A character vector of variables that can be combined to create distinct names for each row and column.
* `pretty_print` Boolean (TRUE / FALSE) indicating whether to return the table as a Kable HTML table that bolds statistically significant finding and creates other stylistic changes. Default is FALSE.
* `table_name` Character string to use as the name of the Kable table. Only used if `pretty_print` is TRUE.
* `rate_per_unit` Integer used to calculate the rate per x number of people. For example, the crime rate is the number of crimes per 100,000 people, so 100,000 would be entered. Defaults to 1, which is no adjustment. Only used if format equals 'binomial'.

**Example**

```r
df <- data.frame(year = c(2016, 2016, 2017, 2017),
                 geo_description = c('Forsyth County, NC', 'Guilford County, NC',
                                     'Forsyth County, NC', 'Guilford County, NC'),
                 estimate = c(.66, .63, .88, .48),
                 se = c(.1, .15, .06, .09),
                 success = c(10, 12, 15, 19),
                 trials = c(15, 19, 17, 39))

# binomial data
ff_estimates_ci(df, 'estimate', format = 'binomial',
                 success = 'success', trials = 'trials', var_names = c('year', 'geo_description'))
```

### Data Cleaning

#### Convert County FIPS code to county name

`ff_county_fips`

This function performs a cross-walk between county names and county FIPS codes. It takes NC county FIPS codes and input and outputs the county name.

**Parameters:**
* `county_fips_codes` NC county FIPS codes

**Example:**

```r
df <- data.frame(county_fips_code = c(1, 3, 67),
                 estimate = c(121, 156, 190))
                 
ff_county_fips(df$county_fips_code)

# Use dplyr to directly add a column of county names to a dataset
df %>%
  mutate(county_name = ff_county_fips(.$county_fips_code))
```

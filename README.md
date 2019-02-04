# Package of common functions
The `FFtools` package contains common Forsyth Futures functions.

## Installation
1. Install the `devtools` package, if not already installed: `install.packages('devtools')`.
2. Install the `FFtools` package directly from GitHub:  `devtools::install_github('forsythfuture/FFtools')`

## Functions

### Data Cleaning

`ff_county_fips`

This function performs a cross-walk between county names and county FIPS codes. It takes NC county FIPS codes and input and outputs the county name.

**Parameters:**
* 'county_fips_codes': NC county FIPS codes

**Example:**
````r
df <- data.frame(county_fips_code = c(1, 3, 67),
                 estimate = c(121, 156, 190))
                 
ff_county_fips(df$county_fips_code)

# Use dplyr to directly add a column of county names to a dataset
df %>%
  mutate(county_name = ff_county_fips(.$county_fips_code))
```



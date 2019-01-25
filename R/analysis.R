#' Matrix of significance tests
#'
#' This function returns a square symetrical matrix of all significance tests for all combinations of
#' values. It calcualtes the p-value from either the z test statistic or Chi-Square test statistic.
#' A two-sided significance test is conducted and the null hypothesis is that there is no difference between the two
#' parameters. The matrix length and width equal the number of rows in the data frame.
#'
#' The z-score formula comes from:
#' U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18.
#'
#' The z-scores are then converted to p-values using the R function for generating cumulative PDFs: `pnorm(z_score, lower.tail=FALSE)*2`.
#' The Chi-Square test of proportions uses `prop.test` and extracts the p-values from this function's results.
#'
#' @param data_frame A dataframe containing estimates and either standard errors for z-score test
#'    or successes and trials for a Chi-Square test.
#' @param estimate An integer or float containing the number to conduct significance tests on.
#' @param se Standard error of the estimate. Required if test a z-score test
#' @param test The significance test to conduct. Either "zscore" or "chi-square". Defaults to 'zscore'.
#' @param success The number of successful trials. Required for Chi-Square test.
#' @param trials The total number of trials. Required for Chi-Square test.
#' @param var_names A character vector of variables that can be combined to create
#'     distinct names for each row and column.
#' @param pretty_print Boolean (TRUE / FALSE) indicating whether to return the table as a Kable HTML table that bolds
#'     statistically significant finding and creates other stylistic changes. Default is FALSE.
#' @param table_name Character string to use as the name of the Kable table. Only used if `pretty_print` is TRUE.
#' @return A square, symmetrical, with a length and width equal the number of rows in the data frame.
#'     Each cell in the matrix contains the results of the significance test from the row in the original
#'     dataframe represented by the column, and the row represented by the row in the matrix. The cell values
#'     signify the p-value of a two-sided test with a null-hypothesis of no difference between the observations.
#' @examples
#' df <- data.frame(year = c(2016, 2017),
#'                  geo_description = c('Forsyth County, NC', 'Guilford County, NC'),
#'                  estimate = c(1,2),
#'                  se = c(.2, .3),
#'                  success = c(10, 12),
#'                  trials = c(15, 19))
#'
#' # Z score test
#' ff_sigtest(data_frame = df, estimate = 'estimate', se = 'se',
#'            test = 'zscore', var_names = c('year', 'geo_description'))
#'
#' # Chi-Square test
#' ff_sigtest(data_frame = df, estimate = 'estimate', success = 'success', trials = 'trials',
#'            test = 'chi-square', var_names = c('year', 'geo_description'))
#' @export
#' @importFrom magrittr "%>%"
ff_sigtest <- function(data_frame, estimate, se, test = 'zscore',
                       success = NULL, trials = NULL, var_names = NULL,
                       pretty_print = FALSE, table_name = NULL) {

  # initialize an empty data frame with one column and the same number
  # of rows as the final dataframe
  sigtest_mat <- data.frame(n = seq(1, nrow(data_frame)))

  if (test == 'zscore') {

    # iterate through each row in the dataframe
    for (i in 1:nrow(data_frame)) {

      # calculate the point estimate differences and the sum of
      # of standard errors for the given row and all other rows
      # this will return a vector
      estimate_diff <- data_frame[[i, estimate]] - data_frame[[estimate]]
      se_diff <- sqrt( data_frame[[i, se]]^2 + data_frame[[se]]^2 )

      # calculate the z score for all row values
      z_score <- abs(estimate_diff / se_diff)
      # convert z score to p-value; multiply by 2 to get two-sided test
      # round to 3 digit places
      p_value <- round( stats::pnorm(z_score, lower.tail=FALSE)*2, 3)

      # add the row of z scores to the z score matrix
      sigtest_mat[, i] <- p_value

    }

  } else if (test == 'chi-square') {

    # make sure there are columns called 'success' and 'trials'
    if (!(('success' %in% colnames(data_frame)) & ('trials' %in% colnames(data_frame)))) {
      stop("The 'success' and 'trials' columns are missing.")
    }

    # create vectors of counts and totals,
    # leads to shorter code than refering to column names
    success_c <- data_frame[[success]]
    trials_c <- data_frame[[trials]]

    # iterate through each row in the dataframe
    for (i in 1:nrow(data_frame)) {

      # conduct proportion test for value at row in loop and all other valyes
      p_value <- sapply(1:nrow(data_frame),
                        function(x) stats::prop.test(c(success_c[i],success_c[x]),c(trials_c[i],trials_c[x]))$p.value)

      # add the row of z scores to the z score matrix
      sigtest_mat[, i] <- round(p_value, 3)

    }

  } else {
    stop("Test must be either 'zscore' or 'chi-square'")
  }

  if (!is.null(var_names)) {

    sigtest_mat <- ff_create_varnames(data_frame, sigtest_mat, var_names)

  }

  # create formatted Kable table if requested
  if (pretty_print == TRUE) {

    # create variable signifying which type of test was run
    # to be used as parameter for next function
    test_type <- if (test == 'zscore') 'continuous' else if (test == 'chi-square') 'binomial'

    return(ff_pretty_kable(data_matrix = sigtest_mat, table_type = 'sigtest',
                           format = test_type, table_name = table_name))

  } else {

    return(sigtest_mat)

  }

}

#' Matrix of estimates and confidence intervals
#'
#' This function returns a square symetrical matrix of all differences between all combinations of rows,
#' along with the 95 percent confidence interval of the difference. For binomial (categorical) datasets, the difference is the
#' percentile difference.
#'
#' @param data_frame A dataframe containing estimates and either standard errors for continuous data
#'    or successes and trials for binomial data.
#' @param estimate An integer or float containing the number to compare differences. Required for continuous format.
#' @param se Standard error of the estimate. Required for continuous format.
#' @param format Type of data; either 'continuous' or 'binomial'. If continuous, the `estimate` columns is
#'     used to generate differences. If binomial, the `success` and `trials` columns are used. Default is continuous.
#' @param success The number of successful trials. Required for binomial format.
#' @param trials The total number of trials. Required for binomial format.
#' @param var_names A character vector of variables that can be combined to create
#'     distinct names for each row and column.
#' @param pretty_print Boolean (TRUE / FALSE) indicating whether to return the table as a Kable HTML table that bolds
#'     statistically significant finding and creates other stylistic changes. Default is FALSE.
#' @param table_name Character string to use as the name of the Kable table. Only used if `pretty_print` is TRUE.
#' @param rate_per_unit Integer used to calculate the rate per x number of people. For example, the crime rate
#'     is the number of crimes per 100,000 people, so 100,000 would be entered. Defaults to 1, which is no adjustment.
#'     Only used if format equals 'binomial'.
#' @return A square, symmetrical, with a length and width equal the number of rows in the data frame.
#'     Each cell in the matrix contains the difference in the estimate of the column minus the row. It
#'     also contains the 95 percent confidence interval of the difference.
#' @examples
#' df <- data.frame(year = c(2016, 2016, 2017, 2017),
#'                  geo_description = c('Forsyth County, NC', 'Guilford County, NC',
#'                                      'Forsyth County, NC', 'Guilford County, NC'),
#'                 estimate = c(.66, .63, .88, .48),
#'                 se = c(.1, .15, .06, .09),
#'                 success = c(10, 12, 15, 19),
#'                 trials = c(15, 19, 17, 39))
#'
#' # binomial data
#' ff_estimates_ci(df, 'estimate', format = 'binomial',
#'                 success = 'success', trials = 'trials', var_names = c('year', 'geo_description'))
#' @export
#' @importFrom magrittr "%>%"
ff_estimates_ci <- function(data_frame, estimate, se, format,
                            success = NULL, trials = NULL, rate_per_unit = 1,
                            var_names = NULL, pretty_print = FALSE, table_name = NULL) {

  # This function takes as input a dataframe with estimates and standard errors,
  # or successes and trials; and calcualted the estimated difference between two
  # point estimates as well as 95 percent CIs

  # initialize an empty data frame with one column and the same number
  # of rows as the final dataframe
  estimate_mat <- data.frame(n = seq(1, nrow(data_frame)))

  if (format == 'continuous') {

    # iterate through each row in the dataframe
    for (i in 1:nrow(data_frame)) {

      # calculate the point estimate differences and the moe
      # for the given row and all other rows this will return a vector
      # must convert to character so estimates and CI can be pasted in single cell
      estimate_diff <- data_frame[[i, estimate]] - data_frame[[estimate]]
      moe_diff <- sqrt( data_frame[[i, se]]^2 + data_frame[[se]]^2 ) * 1.96

      # create single cell that has estimate and CIs
      cell_values <- sprintf("%.2f,[%.2f, %.2f]", estimate_diff, estimate_diff-moe_diff, estimate_diff+moe_diff)

      # add the row of z scores to the z score matrix
      estimate_mat[, i] <- cell_values

    }

  } else if (format == 'binomial') {

    # create vectors of counts and totals,
    # leads to shorter code than refering to column names

    # multiply successes by rate to convert to rate per x
    success_c <- data_frame[[success]]
    trials_c <- data_frame[[trials]]

    # iterate through each row in the dataframe
    for (i in 1:nrow(data_frame)) {

      # conduct proportion test for between value at row in loop and all other valyes
      cell_value <- sapply(1:nrow(data_frame),
                            function(x) ff_proportions(c(success_c[i],success_c[x]), c(trials_c[i],trials_c[x]), rate_per_unit))

      # add the row of z scores to the z score matrix
      estimate_mat[, i] <- cell_value

    }

  } else {
    stop("format value must be either 'continuous' or 'binomial'.")
  }

  if (!is.null(var_names)) {
    estimate_mat <- ff_create_varnames(data_frame, estimate_mat, var_names)
  }

  # create formatted Kable table if requested
  if (pretty_print == TRUE) {

    return(ff_pretty_kable(data_matrix = estimate_mat, table_type = 'estimate',
                           format = format, table_name = table_name))

  } else {

    return(estimate_mat)

  }

}

#' Difference and confidence interval of difference between two binomial observations
#'
#' This function calculates the percentile difference between two observations that
#' are binomial (true / false, yes / no, etc) in nature. It also calculates the confidence
#' interval of this difference. The function is used in `ff_estiamtes_ci`.
#'
#' @param successes Vector of two numbers that represent the number of successes in each
#'     of the observations.
#' @param trials Vector of two numbers that represent the number of trials in each
#'     of the observation
#' @param rate_per_unit Integer used to calculate the rate per x number of people. For example, the crime rate
#'     is the number of crimes per 100,000 people, so 100,000 would be entered. Defaults to 1, which is no adjustment.
#'     Only used if format equals 'binomial'.
#'
ff_proportions <- function(successes, trials, rate_per_unit) {

  # conduct prop test to create percentages for each variable
  # and confidence interval of difference
  pt <- stats::prop.test(x = successes, n = trials)

  # find difference in percentages and convert to rate if needed
  diff <- (pt$estimate[[1]] - pt$estimate[[2]]) * rate_per_unit

  # paste together difference and confidence intervals
  cell_values <- sprintf("%.2f, \n[%.2f, %.2f]",
                         diff, pt$conf.int[[1]]*rate_per_unit, pt$conf.int[[2]]*rate_per_unit)

  return(cell_values)

}

#' Pretty formatting of significance testing nd estimate tables with Kable
#'
#' This function elegantly formats significance testing matrices produces
#' by `ff_sigtest` or estimate matrices produced by `ff_estimates_ci`.
#' It bolds cells that are statistically significant (for significance testing matrices), only
#' keeps rows for Forsyth County, and makes other minor stylistic changes.
#'
#' @param data_matrix A matrix produced by `ff_sigtest` or `ff_estimates_ci`.
#' @param table_type Either 'sigtest' or 'estimate'. Specifies whether the table was generated by
#'     `ff_sigtest` and has results from a significance test; or whether the table was generated
#'     by `ff_estimates_ci` and has estimates and confidence intervals.
#' @param format Type of data; either 'continuous' or 'binomial'. Defaults to 'continuous'.
#' @param table_name A string of characters representing the table name. It is displayed above the table.
#'     Defautls to no table name.
#' @return An html-based Kable table.
#' @importFrom magrittr "%>%"
ff_pretty_kable <- function(data_matrix, table_type, format = 'continuous',
                            table_name = NULL) {

  # return error is table_type is not sigtest or estimate
  if (!((table_type == 'sigtest') | (table_type == 'estimate'))) {
    stop("table_type must be either 'sigtest' or 'estimate'.")
  }

  # for significance testing tables we need to bold statistically significant results
  if (table_type == 'sigtest') {

    # ensure format is either continuous or binomial
    if (!((format == 'continuous') | (format == 'binomial'))) {
      stop("format must be either 'continuous' or 'binomial'.")
    }

    # bold any p-values at 0.05 or lower, which signifies statistical significance.
    thresh <- 0.05

    # bold any cell less than or equal to significance threshold
    data_matrix <- data_matrix %>%
      dplyr::mutate_all(dplyr::funs(
        kableExtra::cell_spec(., bold = ifelse(. <= thresh, T, F))))

  }

  data_matrix <- data_matrix %>%
    # add column names as the first row because row names do not print
    dplyr::mutate(compare_cols = colnames(.),
           # bold column of column / row names
           compare_cols = kableExtra::cell_spec(compare_cols, bold = T)) %>%
    # only keep rows of Forsyth County
    dplyr::filter(stringr::str_detect(compare_cols, 'Forsyth')) %>%
    # make the comparison column (column and row names) the first column
    dplyr::select(compare_cols, dplyr::everything()) %>%
    # create kable table
    knitr::kable(caption = table_name, escape = F)  %>%
    # add formating (every other row in gray)
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                              full_width = F, position = "left", font_size = 10) %>%
    # bold row names
    kableExtra::column_spec(1, bold = T)

  return(data_matrix)

}

#' Create variable names for tables
#'
#' This function creates variable names for tables when `var_names = TRUE` in
#' `ff_sigtest` and `ff_estimate_ci`. Users will not call this function, it will
#' be called within `ff_sigtest` and `ff_estimate_ci`.
#'
#' @param data_frame A data frame used in the function to create the table.
#' @param table_data The significance test or estimate table created by `ff_sigtest` or `ff_estimate_ci`.
#' @param var_names The variable names used inthe function to create the table.
#'
#' @importFrom magrittr "%>%"
ff_create_varnames <- function(data_frame, table_data, var_names) {

  # if there is only one variable name, then use this as the label
  # otherwise paste together variable names
  if (length(var_names) == 1) {

    # sometime isolating a column returns a data frame, and sometimes it returns a vector
    # if a dataframe is returned, isolate first, and only, column as a vector
    if (is.data.frame(unique(data_frame[ , var_names])) == TRUE) {

      names_vec <- unique(data_frame[ , var_names])[[1]]

    } else {

      names_vec <- unique(data_frame[ , var_names])

    }

  } else {

    # create vector of label names by pasting columns together
    names_vec <- apply( data_frame[ , var_names], 1, paste, collapse = ": " )

  }

  # shorted names so they appear cleaner and shorter in the matrix as column and row headers

  # replace any United States and North Carolina values with NC and US
  names_vec <- stringr::str_replace_all(names_vec, 'United States', 'US') %>%
    stringr::str_replace_all('North Carolina', 'NC') %>%
    stringr::str_replace_all(' County, NC', '') %>%
    # replace ethnicities with abbreviation
    stringr::str_replace_all('African American', 'AA') %>%
    stringr::str_replace_all('Hispanic/Latino', 'HL') %>%
    stringr::str_replace_all('White, non-Hispanic', 'Wh') %>%
    # shorten age descriptions (take off the word 'year')
    stringr::str_replace_all(' years', '') %>%
    stringr::str_replace_all(' and over', '+') %>%
    # shorten age by converting 'to' to '-'
    stringr::str_replace_all(' to ', '-') %>%
    # remove word 'ratio;
    stringr::str_replace_all(' ratio', '')

  # add labels as column and row names
  colnames(table_data) <- names_vec
  row.names(table_data) <- names_vec

  return(table_data)

}

#' Plot of p-values
#'
#' This function returns a plot similair to a correlation plot, highlighting whether p-values are significant.
#' THe function takes as input a table of p-values created by `ff_sigtest`. In using `ff_sigtest` to create a plot
#' with this function, the `pretty_print` parameter msut be set to false.
#'
#' @param pvalue_matrix A table of p-values generated from `ff_sigtest`.
#' @return A ggplot graphic that is color coded based on teh p-value.
#' @examples
#' df <- data.frame(year = c(2016, 2016, 2017, 2017),
#'                  geo_description = c('Forsyth County, NC', 'Guilford County, NC',
#'                                      'Forsyth County, NC', 'Guilford County, NC'),
#'                 estimate = c(.66, .63, .88, .48),
#'                 se = c(.1, .15, .06, .09),
#'                 success = c(10, 12, 15, 19),
#'                 trials = c(15, 19, 17, 39))
#'
#' # create table of p-values
#' pvalues <- ff_sigtest(data_frame = df, estimate = 'estimate', se = 'se',
#'                       test = 'zscore', var_names = c('year', 'geo_description'))
#'
#' # generate plot
#' ff_p_plot(pvalues)
#'
#' @export
#' @importFrom magrittr "%>%"
ff_p_plot <- function(pvalue_matrix) {

  # identify column names of first and last columns, so they can be used to gather data
  first <- names(pvalue_matrix[1])
  last <- names(pvalue_matrix[ncol(pvalue_matrix)])

  # add column of row names to matrix
  pvalue_matrix$row_names <- row.names(pvalue_matrix)

  # gather values to create long form data set
  pvalue_matrix %>% tidyr::gather(first:last, key = 'var1', value = 'value') %>%
    # plot
    ggplot2::ggplot(ggplot2::aes(row_names, var1, fill = value))+
    ggplot2::geom_tile(color = "white")+
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0.05, space = "Lab",
                         name="P-value Matrix\nBlue > 0.05\nRed > 0.05") +
    ggplot2::labs(title = 'Statistically significant P-values are in red',
         x = '', y = '') +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                     size = 12, hjust = 1)) +
    ggplot2::coord_fixed()

}

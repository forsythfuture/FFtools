#' Matrix of significance tests
#'
#' This function returns a square symetrical matrix of of all significance tests for all combinations of
#' values. It can calculate either a z-score or a p-value from a Chi-Square test of proportions.
#' The matrix length and width equal the number of rows in the data frame.
#'
#' The z-score formula comes from: U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18
#' The Chi-Square test of proportions uses `prop.test`
#'
#' @param data_frame A dataframe containing estimates and either standard errors for z-score test
#'    or successes and trials for a Chi-Square test.
#' @param estimate An integer or float containing the number to conduct significance tests on.
#' @param se Standard error of the estimate. Required if test a z-score test
#' @param test The significance test to conduct. Either "zscore" or "chi-square". Defaults to 'zscore'.
#' @param success The number of successful trials. Required for Chi-Square test.
#' @param trials The total number of trials. Required for Chi-Square test.
#' @param var_names a character vector of variables that can be combined to create
#'     distinct names for each row and column.
#' @return A square, symmetrical, with a length and width equal the number of rows in the data frame.
#'     Each cell in the matrix contains the results of the significance test from the row in the original
#'     dataframe represented by the column, and the row represented by the row in the matrix.
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
ff_sigtest <- function(data_frame, estimate, se, test = 'z',
                       success = NULL, trials = NULL, var_names = NULL) {

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

      # calculate the z score for all row values, rounds to two decimals
      z_score <- abs( estimate_diff / se_diff) %>% round(2)

      # add the row of z scores to the z score matrix
      sigtest_mat[, i] <- z_score

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
                        function(x) prop.test(c(success_c[i],success_c[x]),c(trials_c[i],trials_c[x]))$p.value)

      # add the row of z scores to the z score matrix
      sigtest_mat[, i] <- round(p_value, 3)

    }

  } else {

    stop("Test must be either 'zscore' or 'chi-square'")

  }

  if (!is.null(var_names)) {

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
    colnames(sigtest_mat) <- names_vec
    row.names(sigtest_mat) <- names_vec

  }

  return(sigtest_mat)

}


#' Pretty formatting of significance testing tables with Kable
#'
#' This function elegantly formats significance testing matrices produces
#' by `ff_sigtest`. It bolds cells that are statistically significant, only
#' keeps rows for Forsyth County, and makes other minor stylistic changes.
#'
#' @param sigtest_matrix A matrix produced by `ff_sigtest`.
#' @param test The significance test to conduct. Either "zscore" or "chi-square". Defaults to 'zscore'.
#'     Test must be the same test used in `ff_sigtest`.
#' @param table_name A string of characters representing the table name. It is displayed above the table.
#'     Defautls to no table name.
#' @return An html-based Kable table.
#' @examples
#' df <- data.frame(year = c(2016, 2017),
#'                  geo_description = c('Forsyth County, NC', 'Guilford County, NC'),
#'                  estimate = c(1,2),
#'                  se = c(.2, .3),
#'                  success = c(10, 12),
#'                  trials = c(15, 19))
#'
#' # Use ff_sigtest to create significance testing matrix
#' ff_sigtest(data_frame = df, estimate = 'estimate', se = 'se',
#'            test = 'zscore', var_names = c('year', 'geo_description')) %>%
#'            # Pipe significance testing matrix into function creating kable table
#'            ff_sigtest_kable(sigtest_matrix = ., test = 'zscore', table_name = 'Example table')
#'
ff_sigtest_kable <- function(sigtest_matrix, test = 'zscore',
                             table_name = NULL) {

  # for z-score we want to bold anything over 1.96,
  # for chi-square, we want to bold anything under 0.05
  # each of these values represent the significance threshold
  thresh <- if (test == 'zscore') 1.96 else 0.05

  # we want to bold numbers over threshold for z and under threshold for chi-square
  # due to this difference, we must create TRUE and FALSE values of whether to bold
  # depending on what test is used
  if_true_bold <- if (test =='zscore') T else F
  if_false_bold <- if (test =='zscore') F else T

  sigtest_matrix %>%
    # bold any z score over 1.96
    dplyr::mutate_all(funs(cell_spec(.,
                              bold = ifelse(. > thresh,
                                            if_true_bold,
                                            if_false_bold)))) %>%
    # add column names as the first row because row names do not print
    dplyr::mutate(Compare = colnames(.),
           # bold column of column / row names
           Compare = cell_spec(Compare, bold = T)) %>%
    # only keep rows of Forsyth County
    dplyr::filter(str_detect(Compare, 'Forsyth')) %>%
    # make the comparison column (column and row names) the first column
    dplyr::select(Compare, everything()) %>%
    # create kable table
    knitr::kable(caption = table_name, escape = F)  %>%
    # add formating (every other row in gray)
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                              full_width = F, position = "left", font_size = 10) %>%
    # bold row names
    kableExtra::column_spec(1, bold = T)
}

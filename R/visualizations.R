#' Bar and Line Chart of Comparison Geographies
#'
#' This function creates a bar and line chart of comparison communities for a given aggregate metric.
#' The most recent year's data will be returned in the bar chart.
#'
#' The input dataset needs to have columns of the following form, and with the following names:
#' - year: year of the data
#' - geo_description: geography
#' - type: general description of the demographic identifier (Race and Ethnicity, Total Population, etc)
#' - subtype: specific description of the demographic identifier (African American, Employment Rate, etc)
#' - estimate: value
#'
#' @param data Name of the dataframe with columns outlined above
#' @param comparison_filter String in the `type` column that signifies the rows that are needed for this plot,
#'                          Will generally be rows of total aggregate value
#' @param line_axis_title Title for y axis and tooltip of line chart
#' @param bar_axis_title Title for x axis and tooltip for bar chart
#' @param percent Boolean (TRUE / FALSE), whether estimate value is a percent; this will add percent labels to axis
#' @param dollar Boolean (TRUE / FALSE), whether estimate value is a dollar; this will add dollar labels to the axis
#' @param geography_order Vector of strings that sets the geography order of the legend
#'
#' @return bar plot of the most recent year of data followed by a line chart
#'
#' @examples
#' geographies <- c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
#'                  "North Carolina", "United States")
#' years <- seq(2006, 2017, 1)
#'
#' df <- data.frame(
#'     year = rep(years, each=length(geographies)),
#'     geo_description = rep(geographies, length(years)),
#'     type = "Total Population",
#'     subtype = "Employment Rate",
#'     stringsAsFactors = FALSE
#' )
#'
#' df$estimate <- rnorm(nrow(df), mean = .5, sd = .15)
#'
#' ff_plot_compare(data = df,
#'                 comparison_filter = "Total Population",
#'                 line_axis_title = "Employment Rate (%)",
#'                 bar_axis_title = "2017 Employment Rate (%)",
#'                 percent = T,
#'                 dollar = F,
#'                 geography_order = c("Forsyth County, NC", "Guilford County, NC",
#'                                     "Durham County, NC",
#'                                     "North Carolina", "United States"))
#' @export
#' @importFrom magrittr "%>%"
ff_plot_compare <- function(data, comparison_filter, line_axis_title, bar_axis_title, percent = F, dollar = F,
                            geography_order = c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
                                                "North Carolina", "United States")) {

   # create bar and line plots separately
   bar <- FFtools::ff_plot_compare_bar(data, comparison_filter, bar_axis_title, percent, dollar, geography_order)
   line <- FFtools::ff_plot_compare_line(data, comparison_filter, line_axis_title, percent, dollar, geography_order)

   highcharter::hw_grid(list(bar, line), ncol = 1) %>%
      htmltools::browsable()

}

#' Bar Chart of Comparison Geographies
#'
#' This function creates a bar chart of Forsyth County and comparison communities for a given metric.
#' The most recent year's data will eb returned in the chart.
#'
#' The input dataset needs to have columns of the following form, and with the following names:
#' - year: year of the data
#' - geo_description: geography
#' - type: general description of the demographic identifier (Race and Ethnicity, Total Population, etc)
#' - subtype: specific description of the demographic identifier (African American, Employment Rate, etc)
#' - estimate: value
#'
#' @param data Name of the dataframe with columns outlined above
#' @param comparison_filter String in the `type` column that signifies the rows that are needed for this plot,
#'                          Will generally be rows of total aggregate value
#' @param y_axis_title Title for y axis and tooltip
#' @param percent Boolean (TRUE / FALSE), whether estimate value is a percent; this will add percent labels to axis
#' @param dollar Boolean (TRUE / FALSE), whether estimate value is a dollar; this will add dollar labels to the axis
#' @param geography_order Vector of strings that sets the geography order of the legend
#'
#' @return bar plot of the most recent year of data
#'
#' @examples
#' geographies <- c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
#'                  "North Carolina", "United States")
#' years <- seq(2006, 2017, 1)
#'
#' df <- data.frame(
#'     year = rep(years, each=length(geographies)),
#'     geo_description = rep(geographies, length(years)),
#'     type = "Total Population",
#'     subtype = "Employment Rate",
#'     stringsAsFactors = FALSE
#' )
#'
#' df$estimate <- rnorm(nrow(df), mean = .5, sd = .15)
#'
#' ff_plot_compare_bar(data = df,
#'                     comparison_filter = "Total Population",
#'                     y_axis_title = "Employment Rate (%)",
#'                     percent = T,
#'                     dollar = F,
#'                     geography_order = c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
#'                                          "North Carolina", "United States"))
#' @export
#' @importFrom magrittr "%>%"
ff_plot_compare_bar <- function(data, comparison_filter, y_axis_title, percent = F, dollar = F,
                                geography_order = c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
                                                    "North Carolina", "United States")) {

   # create color palette for the lines
   set2 <- colorspace::qualitative_hcl(length(geography_order), palette = "Dark 3", alpha = .6)

   # start bar chart function --------------
   bar_data <- data %>%
   # only keep the most recent year # #4b81bf # #969696
   dplyr::filter(year == max(year),
   type == !! comparison_filter) %>%
   # order the geograhy's by the order specified in the parameter
   dplyr::slice(match(!! geography_order, geo_description)) %>%
   # make Forsyth County black so that it stands out
   dplyr::mutate(color = ifelse(stringr::str_detect(geo_description, "Forsyth"),
                         "#426fc2", "#919191")) %>%
   dplyr::select(geo_description, y = estimate, color)

   # multiply estimate times 100 if percent equals true,
   # needed since highcharter plots exact number
   if (percent == TRUE) {
       bar_data$y <- bar_data$y * 100
   }

   chart <- highcharter::highchart() %>%
     highcharter::hc_xAxis(categories = bar_data$geo_description) %>%
     highcharter::hc_yAxis(title = list(text = y_axis_title)) %>%
     highcharter::hc_add_series(bar_data, name = y_axis_title,
                   showInLegend = FALSE) %>%
     highcharter::hc_chart(type="bar") %>%
     highcharter::hc_size(height = 250)

     # if numebr is a percent, add percent notation to y axis and tooltip
     if (percent == TRUE) {
         chart <- chart %>%
         highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
         highcharter::hc_tooltip(valueSuffix = "%")
       } else if (dollar == TRUE) {
           chart <- chart %>%
           highcharter::hc_yAxis(labels = list(format = "${value}")) %>%
           highcharter::hc_tooltip(valuePrefix = "$")
         }

      return(chart)

     }

#' Line Chart of Comparison Geographies
#'
#' This function creates a line chart of Forsyth County and comparison communities for a given metric.
#'
#' The input dataset needs to have columns of the following form, and with the following names:
#' - year: year of the data
#' - geo_description: geography
#' - type: general description of the demographic identifier (Race and Ethnicity, Total Population, etc)
#' - subtype: specific description of the demographic identifier (African American, Employment Rate, etc)
#' - estimate: value
#'
#' @param data Name of the dataframe with columns outlined above
#' @param comparison_filter String in the `type` column that signifies the rows that are needed for this plot,
#'                          Will generally be rows of total aggregate value
#' @param y_axis_title Title for y axis and tooltip
#' @param percent Boolean (TRUE / FALSE), whether estimate value is a percent; this will add percent labels to axis
#' @param dollar Boolean (TRUE / FALSE), whether estimate value is a dollar; this will add dollar labels to the axis
#' @param geography_order Vector of strings that sets the geography order of the legend
#'
#' @return line chart of all years data
#'
#' @examples
#' geographies <- c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
#'                  "North Carolina", "United States")
#' years <- seq(2006, 2017, 1)
#'
#' df <- data.frame(
#'     year = rep(years, each=length(geographies)),
#'     geo_description = rep(geographies, length(years)),
#'     type = "Total Population",
#'     subtype = "Employment Rate",
#'     stringsAsFactors = FALSE
#' )
#'
#' df$estimate <- rnorm(nrow(df), mean = .5, sd = .15)
#'
#' ff_plot_compare_line(data = df,
#'                     comparison_filter = "Total Population",
#'                     y_axis_title = "Employment Rate (%)",
#'                     percent = T,
#'                     dollar = F,
#'                     geography_order = c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
#'                                          "North Carolina", "United States"))
#' @export
#' @importFrom magrittr "%>%"
ff_plot_compare_line <- function(data, comparison_filter, y_axis_title, percent = F, dollar = F,
                                 geography_order = c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
                                                     "North Carolina", "United States")) {

   # identify years, so they can be added as the x axis
   years <- unique(data$year)

   # create color palette for the lines
   set2 <- colorspace::qualitative_hcl(length(geography_order), palette = "Dark 3", alpha = .4)

   # multiply estimate times 100 if percent equals true,
   # needed since highcharter plots exact number
   if (percent == TRUE) {
      data$estimate <- data$estimate * 100
   }

   # create a dataframe of the data so that it can be added as a 'add_series_list'
   # the output will be a dataframe with each row as a geography and a
   # column with a vector of the geograhy's data
   data_list <- data %>%
      dplyr::filter(type == !! comparison_filter) %>%
      # we change the key to name to have the label in the legend
      dplyr::group_by(name = geo_description) %>%
      # the data in this case is simple, is just .$value column
      dplyr::do(data = .$estimate) %>%
      # order the geograhy's by the order specified in the parameter
      dplyr::slice(match(!! geography_order, name)) %>%
      # create column specifying color of each line for the geography
      dplyr::mutate(color = !!set2) %>%
      # make Forsyth County black so that it stands out
      dplyr::mutate(color = ifelse(stringr::str_detect(name, "Forsyth"),
                                    "#292929", color),
             # make Forsyth County's line width larger so that it stands out
             lineWidth= ifelse(stringr::str_detect(name, "Forsyth"),
                                                   4, 2))

   # create chart
   chart <- highcharter::highchart() %>%
      highcharter::hc_chart(type = "line") %>%
      # add years as categories along x axis
      highcharter::hc_xAxis(categories = years) %>%
      # add y axis title
      highcharter::hc_yAxis(title = list(text = y_axis_title)) %>%
      # add data
      highcharter::hc_add_series_list(data_list) %>%
      # remove point markers from plot
      highcharter::hc_plotOptions(
         series = list(
            marker = list(enabled = FALSE)
         )
      ) %>%
      highcharter::hc_title(text = ".",
               style = list(color = "#ffffff", useHTML = TRUE)) %>%
      highcharter::hc_subtitle(text = ".",
                  style = list(color = "#ffffff", fontSize="22px", useHTML = TRUE))

   # if numebr is a percent or dollar, add percent or dollar notation to y axis and tooltip
   if (percent == TRUE) {
      chart <- chart %>%
         highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
         highcharter::hc_tooltip(valueSuffix = "%")
   } else if (dollar == TRUE) {
      chart <- chart %>%
         highcharter::hc_yAxis(labels = list(format = "${value}")) %>%
         highcharter::hc_tooltip(valuePrefix = "$")
   }

   return(chart)

}

#' Bar and Line Chart of Demographics
#'
#' This function creates a bar and line chart of a given demographic data within Forsyth County.
#'
#' The input dataset needs to have, at a minimum, columns of the following form and with the following names:
#' - year: year of the data
#' - geo_description: geography (needs Forsyth County as one of the geographies)
#' - type: general description of the demographic identifier (Race and Ethnicity, Total Population, etc)
#' - subtype: specific description of the demographic identifier (African American, Employment Rate, etc)
#' - estimate: value
#'
#' @param data Name of the dataframe with columns outlined above
#' @param demographic String in the `type` column that signifies the demographic
#' @param line_axis_title Title for y axis and tooltip of line chart
#' @param bar_axis_title Title for x axis and tooltip for bar chart
#' @param percent Boolean (TRUE / FALSE), whether estimate value is a percent; this will add percent labels to axis
#' @param dollar Boolean (TRUE / FALSE), whether estimate value is a dollar; this will add dollar labels to the axis
#'
#' @return bar and line plot of the most recent year of data
#'
#' @examples
#' years <- seq(2006, 2017, 1)
#'
#' df <- data.frame(
#'     year = rep(2017, 3),
#'     geo_description = rep("Forsyth County, NC", 3),
#'     type = rep("Race and Ethnicity", 3),
#'     subtype = c("African American", "White", "Hispanic / Latino"),
#'     estimate = rnorm(3, mean = .5, sd = .15),
#'     stringsAsFactors = FALSE
#' )
#'
#' ff_plot_demo(data = df,
#'              demographic = "Race and Ethnicity",
#'              line_axis_title = "Employment Rate (%)",
#'              bar_axis_title = "2017 Employment Rate (%)",
#'              percent = T,
#'              dollar = F)
#' @export
#' @importFrom magrittr "%>%"
ff_plot_demo <- function(data, demographic, line_axis_title, bar_axis_title, percent = F, dollar = F) {

   # create bar and line plots separately
   bar <- FFtools::ff_plot_demo_bar(data, demographic, bar_axis_title, percent, dollar)
   line <- FFtools::ff_plot_demo_line(data, demographic, line_axis_title, percent, dollar)

   highcharter::hw_grid(list(bar, line), ncol = 1) %>%
      htmltools::browsable()

}

#' Bar Chart of Demographics
#'
#' This function creates a bar chart of a given demographic for the most recent year within Forsyth County.
#'
#' The input dataset needs to have, at a minimum, columns of the following form and with the following names:
#' - year: year of the data
#' - geo_description: geography (needs Forsyth County as one of the geographies)
#' - type: general description of the demographic identifier (Race and Ethnicity, Total Population, etc)
#' - subtype: specific description of the demographic identifier (African American, Employment Rate, etc)
#' - estimate: value
#'
#' @param data Name of the dataframe with columns outlined above
#' @param demographic String in the `type` column that signifies the demographic
#' @param y_axis_title Title for y axis and tooltip
#' @param percent Boolean (TRUE / FALSE), whether estimate value is a percent; this will add percent labels to axis
#' @param dollar Boolean (TRUE / FALSE), whether estimate value is a dollar; this will add dollar labels to the axis
#'
#' @return bar plot of the most recent year of data
#'
#' @examples
#' years <- seq(2006, 2017, 1)
#'
#' df <- data.frame(
#'    year = rep(seq(2007, 2017, 1), each=3),
#'    geo_description = "Forsyth County, NC",
#'    type = "Race and Ethnicity",
#'    subtype = rep(c("African American", "White", "Hispanic / Latino"),11),
#'    stringsAsFactors = FALSE
#' )
#'
#' ff_plot_demo_bar(data = df,
#'                  demographic = "Race and Ethnicity",
#'                  y_axis_title = "Employment Rate (%)",
#'                  percent = T,
#'                  dollar = F)
#' @export
#' @importFrom magrittr "%>%"
ff_plot_demo_bar <- function(data, demographic, y_axis_title, percent = F, dollar = F) {

   bar_data <- data %>%
      # only keep the most recent year
      dplyr::filter(year == max(year),
                   stringr::str_detect(geo_description, "Forsyth"),
                   type == !! demographic) %>%
      dplyr::select(subtype, y = estimate)

   # create color palette for the bars
   bar_data$color <- colorspace::qualitative_hcl(length(unique(bar_data$subtype)), palette = "Dark 2")

   # multiply estimate times 100 if percent equals true,
   # needed since highcharter plots exact number
   if (percent == TRUE) {
      bar_data$y <- bar_data$y * 100
   }

   chart <- highcharter::highchart() %>%
      highcharter::hc_xAxis(categories = bar_data$subtype) %>%
      highcharter::hc_yAxis(title = list(text = y_axis_title)) %>%
      highcharter::hc_add_series(bar_data, name = y_axis_title,
                    showInLegend = FALSE) %>%
      highcharter::hc_chart(type="bar") %>%
      highcharter::hc_size(height = 250)

   # if number is a percent, add percent notation to y axis and tooltip
   if (percent == TRUE) {
      chart <- chart %>%
         highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
         highcharter::hc_tooltip(valueSuffix = "%")
   } else if (dollar == TRUE) {
      chart <- chart %>%
         highcharter::hc_yAxis(labels = list(format = "${value}")) %>%
         highcharter::hc_tooltip(valuePrefix = "$")
   }

   return(chart)

}

#' Line Chart of Demographics
#'
#' This function creates a line chart of the given demographic for Forsyth County.
#'
#' The input dataset needs to have, at a minimum, columns of the following form and with the following names:
#' - year: year of the data
#' - geo_description: geography (needs Forsyth County as one of the geographies)
#' - type: general description of the demographic identifier (Race and Ethnicity, Total Population, etc)
#' - subtype: specific description of the demographic identifier (African American, Employment Rate, etc)
#' - estimate: value
#'
#' @param data Name of the dataframe with columns outlined above
#' @param demographic String in the `type` column that signifies the demographic
#' @param y_axis_title Title for y axis and tooltip
#' @param percent Boolean (TRUE / FALSE), whether estimate value is a percent; this will add percent labels to axis
#' @param dollar Boolean (TRUE / FALSE), whether estimate value is a dollar; this will add dollar labels to the axis
#'
#' @return line chart of the demographic for Forsyth County and all years
#'
#' @examples
#' geographies <- c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
#'                  "North Carolina", "United States")
#' years <- seq(2006, 2017, 1)
#'
#' df <- data.frame(
#'     year = rep(years, each=length(geographies)),
#'     geo_description = rep(geographies, length(years)),
#'     type = "Total Population",
#'     subtype = "Employment Rate",
#'     stringsAsFactors = FALSE
#' )
#'
#' df$estimate <- rnorm(nrow(df), mean = .5, sd = .15)
#'
#' ff_plot_compare_line(data = df,
#'                     comparison_filter = "Total Population",
#'                     y_axis_title = "Employment Rate (%)",
#'                     percent = T,
#'                     dollar = F,
#'                     geography_order = c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
#'                                          "North Carolina", "United States"))
#' @export
#' @importFrom magrittr "%>%"
ff_plot_demo_line <- function(data, demographic, y_axis_title, percent = F, dollar = F) {

   # identify years, so they can be added as the x axis
   years <- unique(data$year)

   # count the number of subgroups within the demographic
   # needed so we know how many colors are needed
   num_colors <- nrow(unique(data[data["type"] == demographic, "subtype"]))

   # create color palette for the lines
   set2 <- colorspace::qualitative_hcl(num_colors, palette = "Set 2")

   # multiply estimate times 100 if percent equals true,
   # needed since highcharter plots exact number
   if (percent == TRUE) {
      data$estimate <- data$estimate * 100
   }

   # create a dataframe of the data so that it can be added as a 'add_series_list'
   # the output will be a dataframe with each row as a geography and a
   # column with a vector of the geograhy's data
   data_list <- data %>%
      # fitler by Forsyth and demographic
      dplyr::filter(stringr::str_detect(geo_description, "Forsyth"),
                    type == !! demographic) %>%
      # we change the key to name to have the label in the legend
      dplyr::group_by(name = subtype) %>%
      # the data in this case is simple, is just .$value column
      dplyr::do(data = .$estimate) %>%
      dplyr::mutate(color = !! set2)

   # create chart
   chart <- highcharter::highchart() %>%
      highcharter::hc_chart(type = "line") %>%
      # add years as categories along x axis
      highcharter::hc_xAxis(categories = years) %>%
      # add y axis title
      highcharter::hc_yAxis(title = list(text = y_axis_title)) %>%
      # add data
      highcharter::hc_add_series_list(data_list) %>%
      # remove point markers from plot
      highcharter::hc_plotOptions(
         series = list(
            marker = list(enabled = FALSE)
         )
      ) %>%
      # add white title to create spacing from bar chart
      highcharter::hc_title(text = ".",
               style = list(color = "#ffffff", useHTML = TRUE)) %>%
      highcharter::hc_subtitle(text = ".",
                  style = list(color = "#ffffff", fontSize="22px", useHTML = TRUE))

   # if number is a percent, add percent notation to y axis and tooltip
   if (percent == TRUE) {
      chart <- chart %>%
         highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
         highcharter::hc_tooltip(valueSuffix = "%")
   } else if (dollar == TRUE) {
      chart <- chart %>%
         highcharter::hc_yAxis(labels = list(format = "${value}")) %>%
         highcharter::hc_tooltip(valuePrefix = "$")
   }

   return(chart)

}

#' Choropleth Map for Census Tracts
#'
#' This function creates choropleth maps for census tract estimates.
#'
#' @param data Name of the dataframe with data and shapefiles
#' @param col_census_name column name as a string of the column with the census tract names
#' @param col_estimate column name as a string of the column with the estimate we want to plot
#' @param title title of plot
#' @param reverse reverse the mapping of the colors to numbers (by default dark colors represent high numbers)
#'
#' @return choropleth map of census tracts and estimate
#'
#' @examples
#' df <- tidycensus::get_acs(state = "NC",
#'                           county = "Forsyth",
#'                           geography = "tract",
#'                           variables = "S2301_C03_001",
#'                           geometry = TRUE) %>%
#'       dplyr::mutate(NAME = stringr::str_replace(", Forsyth County, North Carolina", ""))
#'
#' ff_plot_census(df, "NAME", "estimate", "Employment Rate", T, NULL, "%")
#'
#' @export
#' @importFrom magrittr "%>%"
ff_plot_census <- function(data, col_census_name, col_estimate, title,
                           reverse_colors = F, legend_suffix = NULL, legend_prefix = NULL) {

   title <- title

   pal <- leaflet::colorNumeric(
      palette = "Blues",
      domain = NULL,
      na.color = "transparent",
      reverse = reverse_colors)

   labs <- glue::glue("<strong>{data[[col_census_name]]}</strong><br>{title}: <strong>{data[[col_estimate]]}%</strong>") %>%
      lapply(htmltools::HTML)

   data %>%
      sf::st_transform(crs = '+proj=longlat +datum=WGS84') %>%
      leaflet::leaflet() %>%
      leaflet::setView(lat = 36.0999, lng = -80.2442, zoom = 11) %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::addPolygons(fillOpacity = .8,
                  weight = 1,
                  color = "black",
                  dashArray = "1",
                  smoothFactor = 0.2,
                  fillColor = ~pal(estimate),
                  label = labs,
                  highlight = leaflet::highlightOptions(
                     weight = 5,
                     color = "#666",
                     dashArray = "",
                     fillOpacity = 1,
                     bringToFront = TRUE)) %>%
      leaflet::addLegend(pal = pal, values = ~estimate, opacity = 0.7,
                title = title, position = "bottomright",
                labFormat = leaflet::labelFormat(suffix = legend_suffix,
                                        prefix = legend_prefix))

}

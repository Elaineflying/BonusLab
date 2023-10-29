#' Visualize Airport Delays
#' @description This function creates a plot that visualizes the mean delay of flights for different airports by longitude and latitude using ggplot2.
#' @details The function loads the nycflights13 dataset, filters out missing latitude and longitude values, calculates the mean delay for each airport, and then creates a scatterplot.
#' @return A ggplot2 plot visualizing the mean flight delay by airport location.
#' @examples
#' visualize_airport_delays()
#' @import ggplot2
#' @import dplyr
#' @import nycflights13
#' @importFrom magrittr %>%
#' @export visualize_airport_delays
#'
visualize_airport_delays <- function() {
  # Load the nycflights13 dataset
  flights <- nycflights13::flights
  airports <- nycflights13::airports

  # Filter out rows with missing latitude and longitude
  airports <- airports %>%
    filter(!is.na(lat), !is.na(lon))

  # Join flights and airports datasets to get delay information for each airport
  airport_delays <- flights %>%
    left_join(airports, by = c("dest" = "faa")) %>%
    group_by(dest, lat, lon) %>%
    summarize(mean_delay = mean(dep_delay+arr_delay, na.rm = TRUE))

  # Create a scatterplot using ggplot2
  ggplot(airport_delays, aes(x = lon, y = lat, color = mean_delay)) +
    geom_point(size = 3) +
    scale_color_gradient(name = "Mean Delay", low = "blue", high = "red") +
    labs(title = "Mean Flight Delay by Airport Location", x = "Longitude", y = "Latitude") +
    theme_minimal()
}



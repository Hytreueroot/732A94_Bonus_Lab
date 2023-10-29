#' Airport Delay Visualize
#' 
#' @name visualize_airport_delays
#' 
#' @description
#' Function creates a plot that visualize the mean delay of flights
#' for different airports by longitude and latitude using ggplot2 library.
#' 
#' @examples visualize_airport_delays()
#' 
#' @import nycflights13
#' @import dplyr
#' @import ggplot2
#' 
#' @return Return plot
#' @export visualize_airport_delays

visualize_airport_delays <- function(){
  
  flight <- nycflights13::flights
  airport <- nycflights13::airports
  lat <- airport$lat
  lon <- airport$lon
  dep_delay <- flight$dep_delay
  arr_delay <- flight$arr_delay
  
  arr_delay <- flight %>%
    dplyr::inner_join(airport, by=c("dest"="faa")) %>%
    dplyr::group_by(lat, lon) %>%
    dplyr::summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE), .groups = 'drop')
  
  dep_delay <- flight %>%
    dplyr::inner_join(airport, by=c("origin"="faa")) %>%
    dplyr::group_by(lat, lon) %>%
    dplyr::summarise(mean_dep_delay = mean(dep_delay, na.rm = TRUE), .groups = 'drop')
  
  delays <- dplyr::full_join(dep_delay, arr_delay, by = c("lat", "lon"))
  delay_time <- rowMeans(delays[, c("mean_dep_delay", "mean_arr_delay")], na.rm = TRUE)
  
  delay_plot <- ggplot2::ggplot(delays, aes(x = lon, y = lat, size = delay_time, color = delay_time)) +
    geom_point() +
    labs(title = "Mean of Delays of Flights", x= "Longitude", y = "Latitude", size = "Mean Delay Time (Color)", color = "Mean Delay Time (Shape)")
  
  return(delay_plot)
  
}
#' Calculate Haversine Distance Between Two Points on Earth
#'
#' @description
#' This function calculates the great-circle distance between two points on the Earth's surface,
#' given their longitude and latitude in decimal degrees.
#' It uses the Haversine formula, which accounts for the Earth's curvature.
#'
#' @param lon1 Longtitude of the first point in decimal degrees.
#' @param lat1 Latitude of the first point in decimal degrees.
#' @param lon2 Longitude of the second point in decimal degrees.
#' @param lat2 Latitude of the second point in decimal degrees.
#' @param R The radius of the Eart in kilometers.
#'
#' @return The distance between the two points in kilometers.

#' @examples
#' haversine(-73.9851, 40.7580, -0.1278, 51.5074)
#' # Distance between NY City and London.
#'
#' @export
haversine <- function(lon1,lat1,lon2,lat2, R = 6371){
  # degrees to radians
  lon1 <- lon1 * pi/180
  lat1 <- lat1 * pi/180
  lon2 <- lon2 * pi/180
  lat2 <- lat2 * pi/180

  # H formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat /2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  H <- 2 * R * asin(sqrt(a))
  H
}

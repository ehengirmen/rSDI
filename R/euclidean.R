#' Calculate Euclidean Distance Between Two Points
#'
#' @description
#' This function calculates the Euclidean distance between two points. The Euclidean is the 'straight line' distance
#' between two points in a two-dimensional space. This function takes the coordinates of two points (longitude and latitude)
#' and calculates the straight distance between them, assuming flat Earth approximation.
#'
#' @param lon1 Longitude of the first point in decimal degrees.
#' @param lat1 Latitude of the first point in decimal degrees.
#' @param lon2 Longitude of the second point in decimal degrees.
#' @param lat2 Latitude of the second point in decimal degrees.
#'
#' @return The Euclidean distance between the two points.
#'
#' @examples
#' euclidean(-73.9851, 40.7580, -0.1278, 51.5074)
#' # Euclidean distance between New York City and London.
#'
#' @export
euclidean <- function(lon1, lat1, lon2, lat2) {
sqrt((lon2 - lon1)^2 + (lat2 - lat1)^2)
}

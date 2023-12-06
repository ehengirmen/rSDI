#' Calculate Euclidean Distance Between Two Points
#'
#' @description
#' This function calculates the Euclidean distance between two points. The Euclidean is the 'straight line' distance
#' between two points in a two-dimensional space. This function takes the coordinates of two points (longitude and latitude)
#' and calculates the straight distance between them, assuming flat Earth approximation.
#'
#' @param x1 X-coordinate of the first point.
#' @param y1 Y-coordinate of the first point.
#' @param x2 X-coordinate of the second point.
#' @param y2 Y-coordinate of the second point.
#'
#' @return The Euclidean distance between the two points.
#'
#' @examples
#' euclidean(1, 2, 4, 6)
#' # Euclidean distance between points (1, 2) and (4, 6).
#'
#' @export
euclidean <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

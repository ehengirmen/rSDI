#' Distance Calculation for Graph Nodes
#'
#' @description
#' This function calculates distances between each pair of nodes in the graph.
#' It supports both Haversine and Euclidean formula. The function requires a graph object
#' with nodes containing latitude and longitude attributes.
#'
#' @param g An igraph object, with nodes that have 'latitude' and 'longitude' attributes
#' @param formula The distance calculation formula to use, either 'Haversine' or 'Euclidean'.
#'
#' @return A numeric vector with distances calculated between each pair of nodes in the graph.
#'
#' @examples
#' # Assuming 'g' is a graph object with appropriate latitude and longitude attributes for each node
#' dist_calc(g, formula = 'Haversine')
#' dist_calc(g, formula = 'Euclidean')
#'
#' @export
dist_calc <- function(g, formula = 'Haversine'){
  # get edges
  edges_g <- get.edgelist(g)

  # get coordinates
  latitudes <- V(g)$latitude
  names(latitudes) <- V(g)$name
  longitudes <- V(g)$longitude
  names(longitudes) <- V(g)$name

  # distance vector
  distance <- vector(mode = 'numeric')
  if(! formula %in% c('Haversine', 'Euclidean')) stop("Invalid 'formula' argument")
  if (formula == 'Haversine'){
    for (i in 1:(nrow(edges_g))) {
      node1 <- edges_g[i, 1]
      node2 <- edges_g[i, 2]
      distance[i] <- haversine(longitudes[node1], latitudes[node1],
                               longitudes[node2], latitudes[node2])
    }
    distance
  } else if (formula == 'Euclidean'){
    for (i in 1:(nrow(edges_g))) {
      node1 <- edges_g[i, 1]
      node2 <- edges_g[i, 2]
      distance[i] <- euclidean(longitudes[node1], latitudes[node1],
                               longitudes[node2], latitudes[node2])
    }
  }
  distance
}

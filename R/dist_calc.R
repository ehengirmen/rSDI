#' Distance Calculation for Graph Nodes
#'
#' @description
#' This function calculates distances between each pair of nodes in the graph.
#' It supports both Haversine and Euclidean formula. The function automatically selects the formula based on the availabe vertex attributes: 'x' and 'y' for Euclidean distances, 'latitude' and 'longitude' for Haversine distances.
#'
#' @param g An igraph object, with nodes that have a combination of 'latitude' and 'longitude', or 'x' and 'y' vertices attributes.
#' @param formula Optional parameter to specify the distance calculation formula to use, either 'Haversine' or 'Euclidean'. By default `formula = NULL` and if not specified the otherwise, the function automatically determines the formula based on the available vertex attributes. [x, y] => Euclidean, [latitude,longitude] => Haversine. Note that the `g` must have one of this set of vertex attributes.
#'
#' @return An igraph object with an additional edge attribute 'distance' that contains the calculated distances between each pair of nodes.
#'
#' @examples
#' # Assuming 'g' is a graph object with latitude and longitude or x and y attributes for each node.
#' dist_calc(g)
#' dist_calc(g, formula = 'Haversine')
#' dist_calc(g, formula = 'Euclidean')
#' # an overall example
#' flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
#'
#' # user provides x and y vertices
#' nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
#'
#' g<-graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#'
#'
#' dist_calc(g) # eucl. dist. calculated
#' dist_calc(g, formula = 'Euclidean') # calculates euc when asked
#' dist_calc(g, formula = 'Haversine') # error
#'
#' SDI_MODIFIED(g, distance.calculation = 'Euclidean')  # calculates eucl. dist, and then SDI
#' SDI_MODIFIED(g, distance.calculation ='Haversine') # error => latitude & longitude vertices
#'
#' # user provides latitude and longitude vertices instead of x&y
#' nodes<-data.frame(id=c("A","B","C","D"),latitude=c(0,4,0,4),longitude=c(3,0,0,3))
#'
#' g<-graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#'
#' dist_calc(g) # haversine dist calculated
#' dist_calc(g, formula = 'Haversine') # calculated hav when asked specificallly
#' dist_calc(g, formula = 'Euclidean') # error
#'
#' @export
dist_calc <- function(g, formula = NULL) {

  # get edges
  edges_g <- get.edgelist(g)
  # columns may include lower-Capital case letters
  # lower them, change them in g to lowercase
  # then revert this change before returning g
  vertexAttrsOld <-vertex_attr_names(g)
  vertexAttrsNew <- tolower(vertex_attr_names(g))
  names(vertex_attr(g)) <- vertexAttrsNew


  if(is.null(formula) ){
    if (('x' %in% vertexAttrsNew) && ('y' %in% vertexAttrsNew)){
      formula <- 'Euclidean'
    } else if (('latitude' %in% vertexAttrsNew) && ('longitude' %in% vertexAttrsNew)){
      formula <- 'Haversine'
    } else {
      stop("The igraph object must have a combination of 'x' and 'y',
           or 'latitude' and 'longitude' vertex attributes for distance to
           be calculated.")
    }
  } else {
    if(formula == 'Euclidean'){
      if(!('x' %in% vertexAttrsNew) || !('y' %in% vertexAttrsNew)){
        stop("The igraph object must have 'x' and 'y' vertices for distances to be calculated.")
      }
    } else if (formula == 'Haversine'){
      if(!('latitude' %in% vertexAttrsNew) || !('longitude' %in% vertexAttrsNew)){
        stop("The igraph object must have 'latitude' and 'longitude' vertices
             for distances to be calculated.")
      }
    } else {
      stop("The formula must be either 'Haversine' or 'Euclidean'.")
    }
  }

  # check types of vertices
  if (formula == 'Euclidean'){
    if (!is.numeric(V(g)$x) || !is.numeric(V(g)$y)){
      stop("The 'x' and 'y' vertices must be numeric.")
    }
  } else if (formula == 'Haversine'){
    if (!is.numeric(V(g)$latitude) || !is.numeric(V(g)$longitude)){
      stop("The 'latitude' and 'longitude' vertices must be numeric.")
    }
  }

  # initiate dist values
  distH <- vector(length = nrow(edges_g), mode = 'numeric')
  distV <- vector(length = nrow(edges_g),mode = 'numeric')
  distance <- vector(length = nrow(edges_g), mode = 'numeric')

  # check formula
  if (formula == 'Euclidean'){
    distV <- V(g)$x
    distH <- V(g)$y
    # this is needed to accurately calculate distances
    names(distH) <- V(g)$name
    names(distV) <- V(g)$name

    for (i in seq_len(nrow(edges_g))) {
      node1 <- edges_g[i, 1]
      node2 <- edges_g[i, 2]
      distance[i] <- euclidean(
        distV[node1], distH[node1],
        distV[node2], distH[node2]
      )
    }
  } else if (formula == 'Haversine'){
    distV <- V(g)$longitude
    distH <- V(g)$latitude
    # this is needed to accurately calculate distances
    names(distH) <- V(g)$name
    names(distV) <- V(g)$name
    for (i in seq_len(nrow(edges_g))) {
      node1 <- edges_g[i, 1]
      node2 <- edges_g[i, 2]
      distance[i] <- haversine(
        distV[node1], distH[node1],
        distV[node2], distH[node2]
      )
    }
  }
  names(vertex_attr(g)) <- vertexAttrsOld
  E(g)$distance <- distance
  return(g)
}


#' Distance Calculation for Graph Nodes
#'
#' @description
#' This function calculates distances between each pair of nodes in the graph.
#' It supports both Haversine and Euclidean formula. The function automatically selects the formula based on the availabe vertex attributes: 'x' and 'y' for Euclidean distances, 'latitude' and 'longitude' for Haversine distances.
#'
#' @param g An igraph object, with nodes that have 'latitude' and 'longitude', or 'x' and 'y' attributes
#' @param formula Optional parameter to specify the distance calculation formula to use, either 'Haversine', 'Euclidean'. By default `formula = 'NULL'` and if not specified the otherwise, the function automatically determines the formula based on the available vertex attributes. [x, y] => Euclidean, [latitude,longitude] => Haversine. Note that the `g` must have one of this set of vertex attributes.
#'
#' @return A numeric vector with distances calculated between each pair of nodes in the graph.
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
# dist_cal cın SDI fonksiyonuna entegrasyonu  distance.calculation ile yapılıyor
#' SDI_MODIFIED(g, distance.calculation ='Haversine') # calculates haversine dist, and then SDI
#' SDI_MODIFIED(g, distance.calculation = 'Euclidean') # error => requires x&y vertices
#'
#' @export
dist_calc <- function(g, formula = 'NULL') {

  # get edges
  edges_g <- get.edgelist(g)
  # columns may include l-C letter
  vertexAttrs <- tolower(vertex_attr_names(g))

  if(formula == 'Euclidean'){
    if(!('x' %in% vertexAttrs) || !('y' %in% vertexAttrs)){
      stop('The igraph object must have x & y vertices for distances to be calculated.')
    }
  } else if (formula == 'Haversine'){
    if(!('latitude' %in% vertexAttrs) || !('longitude' %in% vertexAttrs)){
      stop('The igraph object must have latitude & longitude vertices for distances to be calculated.')
    }
  } else if(formula == 'NULL'){
    if ('x' %in% vertexAttrs && 'y' %in% vertexAttrs){
      formula <- 'Euclidean'
    } else if ('latitude' %in% vertexAttrs && 'longitude' %in% vertexAttrs){
      formula <- 'Haversine'
    } else {
      formula <- 'Haversine'
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
    # this is needed to accuratly calc dists
    names(distH) <- V(g)$name
    names(distV) <- V(g)$name

    for (i in 1:(nrow(edges_g))) {
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
    # this is needed to accuratly calc dists
    names(distH) <- V(g)$name
    names(distV) <- V(g)$name
    for (i in 1:(nrow(edges_g))) {
      node1 <- edges_g[i, 1]
      node2 <- edges_g[i, 2]
      distance[i] <- haversine(
        distV[node1], distH[node1],
        distV[node2], distH[node2]
      )
    }
  }
  return(distance)
}



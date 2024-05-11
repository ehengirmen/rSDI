#' Not for explicit use.
#'
#' @param g the graph
#' @param mode directionality 'undirected', 'in', 'out', or 'all'
#'
#' @return a vector of values
#' @export
#'
#' #@examples
unweightedAllVerticesSDI <- function (g, mode="all") {
  sdi <- function(v) {unweightedSingleVertexSDI(g,v,mode=mode)}
  unlist(  lapply(igraph::V(g), sdi  )  )
}

#' Not for explicit use.
#'
#' @param g the graph
#'
#' @return a numerical SDI value
#' @export
#'
#' #@examples
unweightedNetworkSDI <- function(g) {
  edges <- igraph::E(g)
  sum(edges$distance)/length(edges)
}

#' Not for explicit use.
#'
#' @param g the graph
#' @param v the vertex
#' @param mode directionality 'undirected', 'in', 'out', or 'all'
#'
#' @return a numerical SDI value
#' @export
#'
#' #@examples
unweightedSingleVertexSDI <- function(g, v, mode=all) {
  vedges<-igraph::incident(g,v,mode=mode)
  if (length(vedges)>0) {
    sum(vedges$distance)/length(vedges)
  } else {
    NA
  }
}

#' Not for explicit use.
#'
#' @param g the graph
#' @param mode directionality 'undirected', 'in', 'out', or 'all'
#'
#' @return a vector of values
#' @export
#'
#' #@examples
weightedAllVerticesSDI <- function (g, mode="all") {
  sdi <- function(v) {weightedSingleVertexSDI(g,v,mode=mode)}
  unlist(  lapply(igraph::V(g), sdi  )  )
}

#' Not for explicit use.
#'
#' @param g the graph
#'
#' @return a numerical SDI value
#' @export
#'
#' #@examples
weightedNetworkSDI <- function(g) {
  edges <- igraph::E(g)
  sum(edges$weight*edges$distance)/sum(edges$weight)
}

#' Not for explicit use.
#'
#' @param g the graph
#' @param v the vertex
#' @param mode directionality 'undirected', 'in', 'out', or 'all'
#'
#' @return a numerical SDI value
#' @export
#'
#' #@examples
weightedSingleVertexSDI <- function(g, v, mode=all) {
  vedges<-igraph::incident(g,v,mode=mode)
  if (length(vedges)>0) {
    sum(vedges$weight*vedges$distance)/sum(vedges$weight)
  } else {
    NA
  }
}

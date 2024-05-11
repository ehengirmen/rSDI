#' Not for explicit use.
#'
#' @param g
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
unweightedAllVerticesSDI <- function (g, mode="all") {
  sdi <- function(v) {unweightedSingleVertexSDI(g,v,mode=mode)}
  unlist(  lapply(V(g), sdi  )  )
}

#' Not for explicit use.
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
unweightedNetworkSDI <- function(g) {
  edges <- E(g)
  sum(edges$distance)/length(edges)
}

#' Not for explicit use.
#'
#' @param g
#' @param v
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
unweightedSingleVertexSDI <- function(g, v, mode=all) {
  vedges<-incident(g,v,mode=mode)
  if (length(vedges)>0) {
    sum(vedges$distance)/length(vedges)
  } else {
    NA
  }
}

#' Not for explicit use.
#'
#' @param g
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
weightedAllVerticesSDI <- function (g, mode="all") {
  sdi <- function(v) {weightedSingleVertexSDI(g,v,mode=mode)}
  unlist(  lapply(V(g), sdi  )  )
}

#' Not for explicit use.
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
weightedNetworkSDI <- function(g) {
  edges <- E(g)
  sum(edges$weight*edges$distance)/sum(edges$weight)
}

#' Not for explicit use.
#'
#' @param g
#' @param v
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
weightedSingleVertexSDI <- function(g, v, mode=all) {
  vedges<-incident(g,v,mode=mode)
  if (length(vedges)>0) {
    sum(vedges$weight*vedges$distance)/sum(vedges$weight)
  } else {
    NA
  }
}

# 5

weightedNetworkSDI <- function(g) {
  edges <- E(g)
  sum(edges$weight*edges$distance)/sum(edges$weight)
}

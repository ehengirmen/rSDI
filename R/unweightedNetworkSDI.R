# 6

unweightedNetworkSDI <- function(g) {
  edges <- E(g)
  sum(edges$distance)/length(edges)
}

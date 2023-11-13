# 4

unweightedAllVerticesSDI <- function (g, mode="all") {
  sdi <- function(v) {unweightedSingleVertexSDI(g,v,mode=mode)}
  unlist(  lapply(V(g), sdi  )  )
}

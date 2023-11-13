# 2


weightedAllVerticesSDI <- function (g, mode="all") {
  sdi <- function(v) {weightedSingleVertexSDI(g,v,mode=mode)}
  unlist(  lapply(V(g), sdi  )  )
}

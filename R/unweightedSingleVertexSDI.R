# 3

unweightedSingleVertexSDI <- function(g, v, mode=all) {
  vedges<-incident(g,v,mode=mode)
  if (length(vedges)>0) {
    sum(vedges$distance)/length(vedges)
  } else {
    NA
  }
}

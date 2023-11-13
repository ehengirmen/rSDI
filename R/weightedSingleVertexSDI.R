# 1

weightedSingleVertexSDI <- function(g, v, mode=all) {
  vedges<-incident(g,v,mode=mode)
  if (length(vedges)>0) {
    sum(vedges$weight*vedges$distance)/sum(vedges$weight)
  } else {
    NA
  }
}

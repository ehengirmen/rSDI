# 7

SDI <- function (g,level="vertex",variant="weighted",mode="all") {
  if (! mode %in% c("in","out","all")) stop("Invalid 'mode' argument")
  if (level=="network") {
    if (variant=="weighted") weightedNetworkSDI(g)
    else if (variant=="unweighted") unweightedNetworkSDI(g)
    else stop("Invalid 'variant' argument")
  } else if (level=="vertex") {
    if (variant=="weighted") weightedAllVerticesSDI(g, mode=mode)
    else if (variant=="unweighted") unweightedAllVerticesSDI(g, mode=mode)
    else stop("Invalid 'variant' argument")
  } else {
    stop("Invalid 'level' argument")
  }
}

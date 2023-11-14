# distance.calculation kısmını SDI fonksiyonuna ekledim

SDI_MODIFIED <- function (g, distance.calculation = NULL, level="vertex",variant="weighted",mode="all") {
  # added distance calculation
  if(!is.null(distance.calculation)){
    if(!distance.calculation %in% c('Haversine', 'Euclidean')) stop("Invalid 'distance.calculation' argument")
    distances <- dist_calc(g, formula = distance.calculation)
    E(g)$distance <- distances
  } else {
    # if null: check if distance is already calculated
    if(is.null(E(g)$distance)){
      stop("No 'distance' attribute found in edges.
           Please provide a 'distance.calculation' method or
           ensure the graph has pre-calculated distances")
    }
  }
  ## rest is the same
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

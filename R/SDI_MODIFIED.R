# base fun:
# SDI(toyGraph, level="vertex", directionality="undirected", weight.use="weighted")

# change variant arg to weight.use
# directionality to be included. for now all considered as directed, thus: n**d**w, n**d**u etc.

SDI_MODIFIED <- function (g, distance.calculation = NULL, level="vertex",variant="weighted",mode="all") {
  # added distance calculation
  if(!is.null(distance.calculation)){
    if(!distance.calculation %in% c('Haversine', 'Euclidean')) stop("Invalid 'distance.calculation' argument")
    g <- dist_calc(g, formula = distance.calculation)
  } else {
    # if null: check if distance is already calculated
    if(is.null(E(g)$distance)){
      warning('No "distance" attribute found in edges. `SDI` computed and
      added edge distance attributes based on coordinate attributes.')
      g <- dist_calc(g)
    }
  }
  ## rest is the same
  if (! mode %in% c("in","out","all")) stop("Invalid 'mode' argument")

  if (level=="network") {
    if (variant=="weighted") {
      V(g)$ndw <- weightedNetworkSDI(g)
      return(g)
    }
    else if (variant=="unweighted"){
      V(g)$ndu <- unweightedNetworkSDI(g)
      return(g)
    }
    else stop("Invalid 'variant' argument")
  } else if (level=="vertex") {
    if (variant=="weighted") {
      V(g)$vdw <- weightedAllVerticesSDI(g, mode=mode)
      return(g)
    }
    else if (variant=="unweighted") {
      V(g)$vdu <- unweightedAllVerticesSDI(g, mode=mode)
      return(g)
    }
    else stop("Invalid 'variant' argument")
  } else {
    stop("Invalid 'level' argument")
  }
}

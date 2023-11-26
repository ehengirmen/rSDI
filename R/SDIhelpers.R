# SDIhelpers are set of functions that simplfy the SDI.R file

SDIcomputer <- function(g, level, weight.use, directionality, mode) {
  if (level=="network") {
  if (weight.use=="weighted") {
    SDI_value <-  weightedNetworkSDI(g)
    g <- set_graph_attr(g, name = 'SDI_nuw', value = SDI_value)
    return(g)
  }
  else if (weight.use=="unweighted"){
    SDI_value <- unweightedNetworkSDI(g)
    g <- set_graph_attr(g, name = 'SDI_nuu', value = SDI_value)
    return(g)
  }
  else stop("Invalid 'weight.use' argument")
} else if (level=="vertex") {
  if (weight.use=="weighted") {
    SDIname <-paste0('SDI_','v',substr(directionality,1,1),'w')
    SDI_value <- weightedAllVerticesSDI(g, mode=mode)
    g <- set_vertex_attr(g, name = SDIname, value = SDI_value )
    return(g)
  }
  else if (weight.use=="unweighted") {
    SDIname <- paste0('SDI_','v',substr(directionality,1,1),'u')
    SDI_value <- unweightedAllVerticesSDI(g, mode=mode)
    g <- set_vertex_attr(g, name = SDIname, value = SDI_value )
    return(g)
  }
  else stop("Invalid 'weight.use' argument")
} else {
  stop("Invalid 'level' argument")
}
  }


variantParser <- function(variant){
  levels <- c('network', 'vertex')
  weights <- c('weighted', 'unweighted')
  directions <- c('undirected', 'in', 'out', 'all')

  givenLevel <- substr(variant,1,1)
  givenDirection <- substr(variant,2,2)
  givenWeight <- substr(variant,3,3)

  if (!givenLevel %in% substr(levels,1,1)){
    stop('Invalid first letter for the variant level: can be either "v"(vertex), or "n"(network).')}
  else if(!givenDirection %in% substr(directions,1,1)){
    stop('Invalid second letter for the variant direction: can be "u"(undirected), "i"(in), or "o"(out)')
  } else if (!givenWeight %in% substr(weights,1,1)){
    stop('Invalid third letter entry for the variant weight: can be either "w"(weighted), or "u"(unweighted).' )
  }

  level <- levels[startsWith(levels, givenLevel)]
  weight.use <- weights[startsWith(weights, givenWeight)]
  directionality <- directions[startsWith(directions, givenDirection)]

  return(list(level = level, weight.use = weight.use, directionality = directionality))
}





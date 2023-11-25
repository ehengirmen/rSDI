# 7

# TO DO: SDI takes data files and creates g then continues...
# check for improvements, potential tests.
# ask about the directioanlity: does undirected mean mode set to 'all', or graph_from_data_frame(..., directed = F)?

SDI <- function (g, distance.calculation = NULL, level="vertex",
                 weight.use="weighted",directionality="undirected",
                 variant = NULL) {
  #------------------------------------------------------
  # If distance calculation method is not given, calculate it and add it to g.
  # Also warn the user about it. Is warning necessary?
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
  #------------------------------------------------------
  # variant arguement
  # if user gives a variant use them as other argument values
  if (!is.null(variant)){
    levels <- c('network', 'vertex')
    weights <- c('weighted', 'unweighted')
    directions <- c('undirected', 'in', 'out', 'all')

    givenLevel <- substr(variant,1,1)
    givenDirection <- substr(variant,2,2)
    givenWeight <- substr(variant,3,3)

    if (!givenLevel %in% substr(levels,1,1)){
      stop('Invalid first letter for the variant level: can be either "u"(undirected), or "d"(directed).')}
    else if (!givenWeight %in% substr(weights,1,1)){
      stop('Invalid second letter entry for the variant weight: can be either "w"(weighted), or "u"(unweighted).' )
    } else if(!givenDirection %in% substr(directions,1,1)){
      stop('Invalid third letter for the variant direction: can be "u"(undirected), "i"(in), or "o"(out)')
      # direction cannot be all right? all is the same as undirected? ASK!!
    }

    level <- levels[startsWith(levels, givenLevel)]
    weight.use <- weights[startsWith(weights, givenWeight)]
    directionality <- directions[startsWith(directions, givenDirection)]

  }
  # I am assuming (i dont know why) undirected directionality is equal to setting mode 'all'
  if (directionality == 'undirected'){
    mode <- 'all'} else {
      mode <- directionality
    }
  #------------------------------------------------------
  # match the arguemnts to relevant functions
  # for network return g with graph attributes
  # for vertex return g with vertex attributes

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

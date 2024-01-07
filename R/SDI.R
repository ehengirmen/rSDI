#' SDI
#'
#' @param flows A data frame or an igraph object
#' @param nodes if flows are data frame, nodes must be supplied as a data frame. If flows are igraph object then not required
#' @param distance.calculation optional method for distance calculation. 'Haversine' or 'Euclidean'. If not provided and edge distances are not available, distances are calculated by the SDI function.
#' @param level The level to calculate the SDI. 'network' or 'vertex'
#' @param weight.use 'weighted', 'unweighted', or 'generalized'
#' @param directionality 'undirected', 'in', 'out', or 'all'
#' @param variant Optional. Instead of specifying the level, weight, and directionality separately, the user can just supply the initial letters of each in that order to this argument.
#' @param alpha Optional parameter used for generalized SDI calculations.
#'
#' @return An i graph object with SDI attributes added. The class of the object includes 'SDI'.
#' @export
SDI <- function (flows, nodes = NULL,  distance.calculation = NULL, level = "vertex",
                          weight.use = "weighted", directionality = "undirected",
                          variant = NULL, alpha = NULL) {
  # check the type of inputs
  # if data frames => crete an igraph obj
  # flows are data frame if nodes are given, else an igraph
  if (!is.null(nodes) && 'data.frame' %in% class(flows)){
    g <- graph_from_data_frame(flows, directed = T, vertices = nodes)
  } else if ('igraph' %in% class(flows)){
    g <- flows
  } else {
    stop('Invalid input: please provide a data frame or an i graph object.')
  }
  # Distance calculation
  if (!is.null(distance.calculation)) {
    g <- dist_calc(g, formula = distance.calculation)
  } else {
    if (is.null(E(g)$distance)) {
      g <- dist_calc(g)
    }
  }
  # Check if a variant is provided
  if (!is.null(variant) && length(variant) > 0) {
    for (v in variant) {
      if (!endsWith(v, 'g')){
        parsedVariant <- variantParser(v)
        if ((parsedVariant$weight.use == 'weighted') & (is.null(E(g)$weight))) {stop('To calculate a weighted index you need to provide "weights" attribute.')}
        g <- SDIcomputer(g, parsedVariant$level, parsedVariant$weight.use, parsedVariant$directionality,
                         if (parsedVariant$directionality == 'undirected') 'all' else parsedVariant$directionality)} else {
                           # generalized variant
                           parsedVariant <- variantParser(v)
                        if (is.null(E(g)$weight)){stop('To calculate a generalized index you need to provide "weights" attribute.')}
                        if (is.null(alpha)){stop('To calculate a generalized index you need to provide a "alpha" in SDI().')}
                           sdiWeighted <- SDIvalue(g, level = parsedVariant$level, mode = if (parsedVariant$directionality == 'undirected') 'all' else parsedVariant$directionality,
                                                   weight.use = 'weighted')

                           sdiUnweighted <- SDIvalue(g, level = parsedVariant$level, mode =if (parsedVariant$directionality == 'undirected') 'all' else parsedVariant$directionality,
                                                     weight.use = 'unweighted')

                           calculatedSDI <- sdiUnweighted^alpha + sdiWeighted^(1-alpha)

                           if (parsedVariant$level == 'network'){
                             g <- set_graph_attr(g, name = paste0('SDI_n', substr(parsedVariant$directionality,1,1),'g'),
                                                 value = calculatedSDI)
                           } else if (parsedVariant$level== 'vertex')
                             g <- set_vertex_attr(g, name = paste0('SDI_v', substr(parsedVariant$directionality,1,1), 'g'),
                                                  value = calculatedSDI)
                         }
    }
  } else {
    # Use the provided level, weight.use, and directionality
    if ((weight.use == 'weighted') & (is.null(E(g)$weight))){stop('To calculate a weighted index you need to provide "weights" attribute.')}
    mode <- if (directionality == 'undirected') 'all' else directionality

    if (!weight.use == 'generalized'){
    g <- SDIcomputer(g, level, weight.use, directionality, mode)
    } else {
      sdiWeighted <- SDIvalue(g, level = level, mode = mode, weight.use = 'weighted')
      sdiUnweighted <- SDIvalue(g, level = level, mode = mode, weight.use = 'unweighted')
      calculatedSDI <- sdiUnweighted^alpha + sdiWeighted^(1-alpha)

      if (level == 'network'){
        g <- set_graph_attr(g, name = paste0('SDI_n', substr(directionality,1,1),'g'),
                            value = calculatedSDI)
      } else if (level== 'vertex')
        g <- set_vertex_attr(g, name = paste0('SDI_v', substr(directionality,1,1), 'g'),
                             value = calculatedSDI)
    }
  }
  class(g) <- c("SDI", class(g))
  return(g)
}



## Functions used
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
  weights <- c('weighted', 'unweighted', 'generalized')
  directions <- c('undirected', 'in', 'out', 'all')

  givenLevel <- substr(variant,1,1)
  givenDirection <- substr(variant,2,2)
  givenWeight <- substr(variant,3,3)

  if (!givenLevel %in% substr(levels,1,1)){
    stop('Invalid first letter for the variant level: can be either "v"(vertex), or "n"(network).')}
  else if(!givenDirection %in% substr(directions,1,1)){
    stop('Invalid second letter for the variant direction: can be "u"(undirected), "i"(in), or "o"(out)')
  } else if (!givenWeight %in% substr(weights,1,1)){
    stop('Invalid third letter entry for the variant weight: can be either "w"(weighted),"u"(unweighted), or "g"(generalized)' )
  }

  level <- levels[startsWith(levels, givenLevel)]
  weight.use <- if (!givenWeight == 'g'){weights[startsWith(weights, givenWeight)]} else {NA}
  directionality <- directions[startsWith(directions, givenDirection)]

  return(list(level = level, weight.use = weight.use, directionality = directionality))
}



SDIvalue <- function(g, level, weight.use, mode) {
  if (level=="network") {
    if (weight.use=="weighted") {
      SDI_value <-  weightedNetworkSDI(g)
      return(SDI_value)
    }
    else if (weight.use=="unweighted"){
      SDI_value <- unweightedNetworkSDI(g)
      return(SDI_value)
    }
    else stop("Invalid 'weight.use' argument")
  } else if (level=="vertex") {
    if (weight.use=="weighted") {
      SDI_value <- weightedAllVerticesSDI(g, mode=mode)
      return(SDI_value)
    }
    else if (weight.use=="unweighted") {
      SDI_value <- unweightedAllVerticesSDI(g, mode=mode)
      return(SDI_value)
    }
    else stop("Invalid 'weight.use' argument")
  } else {
    stop("Invalid 'level' argument")
  }
}






# 7

# TO DO
# 1. SDI takes alpha value.
# 2. improve readability, tests


SDI <- function (flows, nodes = NULL,  distance.calculation = NULL, level = "vertex",
                 weight.use = "weighted", directionality = "undirected",
                 variant = NULL) {
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
      parsedVariant <- variantParser(v)
      g <- SDIcomputer(g, parsedVariant$level, parsedVariant$weight.use, parsedVariant$directionality,
                       if (parsedVariant$directionality == 'undirected') 'all' else parsedVariant$directionality)
    }
  } else {
    # Use the provided level, weight.use, and directionality
    mode <- if (directionality == 'undirected') 'all' else directionality
    g <- SDIcomputer(g, level, weight.use, directionality, mode)
  }
  return(g)
}


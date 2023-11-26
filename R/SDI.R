# 7

# TO DO
# 1. SDI should be able to take data frames and create g from them.
# 2. SDI takes alpha value?


SDI <- function (g, distance.calculation = NULL, level = "vertex",
                 weight.use = "weighted", directionality = "undirected",
                 variant = NULL) {
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


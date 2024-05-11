#' plot.SDI generates a plot of the network and SDI metrics on a geographic map.
#'
#' @param g The igraph object to be plotted, whose vertices have attributes corresponding to SDI metrices.
#' @param variant The SDI variant with a prefix, such as "SDI_vuw", etc.
#' @param circle.size.scale Increase of decrease the size of circles drawn on nodes to represent SDI metric
#' @param circle.color Change color of circles
#' @param edges Whether to draw edges or not
#' @param edge.width.range If edges are to be drawn give a custom range of edge widths
#'
#' @return
#' @export
#'
#' @examples
plot.SDI <- function(g, variant="", circle.size.scale=1, circle.color="red", edges=FALSE, edge.width.range=c(0.01, 0.5)) {
  library(ggraph)
  world_map <- map_data("world")
  z<-get.vertex.attribute(g,paste0("SDI_",variant))
  normalsize <- 7*z/mean(z,na.rm=TRUE)
  lay <- create_layout(g, 'manual', x=V(g)$longitude, y=V(g)$latitude)
  p<- ggraph(lay) +
    geom_path(aes(x = long, y = lat, group = group), data = world_map,  color = "black", na.rm=TRUE) +
    scale_color_identity() +
    scale_x_continuous(limits = c(min(V(g)$longitude)-2, max(V(g)$longitude)+2))+
    scale_y_continuous(limits = c(min(V(g)$latitude)-2, max(V(g)$latitude)+2))+
    geom_node_text(label=V(g)$label, size=2)+
    xlab("") +
    ylab("")
  if (edges) {
    p<- p + geom_edge_link(aes(width = weight/max(weight)),arrow = arrow(length = unit(5, 'mm')))+
      scale_edge_width(range = edge.width.range)
  }
  p+
    geom_node_point(size = normalsize*circle.size.scale, aes(color=circle.color),alpha=0.4)+
    theme(legend.position = "none")
  #p+coord_fixed()
}


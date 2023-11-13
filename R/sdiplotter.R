# 13

sdiplotter <- function (g, sdiskor1=V(g)$sacilim, sdiskor2=NULL, q=0.9, labels=c("Work","School"),links=TRUE) {
  if(!is.null(q)) {
    gd<-delete.edges(g,E(g)[which(E(g)$weight<quantile(E(g)$weight,q))])
  }
  lay <- create_layout(gd, 'manual', x=V(gd)$longitude, y=V(gd)$latitude)
  m<-getmaptr()
  iller<-ym_iller()
  m<-m[order(m$name),][rank(iller$label),]
  cols<-m$name
  p <- ggraph(lay) +
    geom_sf(data=m)#+,aes(fill=cols))
  if (links) {
    p<- p + geom_edge_link(alpha=0.1,arrow = arrow(length = unit(2, 'mm')))
  }
  p<- p + geom_node_point(size = 10*sdiskor1/mean(sdiskor1,na.rm=TRUE), aes(color="red"),alpha=0.4)
  if (is.null(sdiskor2)) {} else {
    p<- p + geom_node_point(size = 10*sdiskor2/mean(sdiskor2,na.rm=TRUE), aes(color="yellow"),alpha=0.4)
  }
  p +
    geom_node_text(label=V(gd)$ILADI, size=2)+
    theme(legend.position = c(0.07, 0.12),legend.title=element_text(size=8), legend.text=element_text(size=7))+
    scale_color_manual(name="SDI variant",
                       values=c("red","yellow"),labels=labels) #+ theme(legend.position="none")
}

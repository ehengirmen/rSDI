# 11


getmaptr <- function() {
  x <- ne_states(country="Turkey", returnclass = "sf")
  x$name[x$name=="Sirnak"]<-"Şırnak"
  x$name[x$name=="Sanliurfa"]<-"Şanlıurfa"
  x
}

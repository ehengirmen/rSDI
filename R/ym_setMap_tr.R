# 9

ym_setMap_tr <- function() {
  require(rnaturalearth)
  x <- ne_states(country="Turkey", returnclass = "sf")
  save(x, file="ilharita.RData")
}

# 12

ym_iller <- function() {
  tmpvertices<-read.csv("yerlesimler-ilceler.csv",encoding="UTF-8")
  tmpvertices[which(tmpvertices$TIP %in% c("Büyükşehir İl Merkezi","İl Merkez İlçesi")),]
}

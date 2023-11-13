# 8

SDIalpha <- function (g,level="vertex",mode="all",alpha=NULL) {
  SDI(g,level=level,variant="unweighted",mode=mode)^alpha+SDI(g,level=level,variant="weighted",mode=mode)^(1-alpha)
}

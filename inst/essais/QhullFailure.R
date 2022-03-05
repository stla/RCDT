library(RCDT)

X <- as.matrix(read.table("inst/essais/points.txt"))

del <- delaunay(X)

plotDelaunay(del, color = "random")

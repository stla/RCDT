library(RCDT)

V <- read.table(system.file("extdata", "face_vertices.txt"))
E <- read.table(system.file("extdata", "face_edges.txt"))
del <- delaunay(
  points = as.matrix(V)[, c(2L, 3L)], edges = as.matrix(E)[, c(2L, 3L)]
)
opar <- par(mar = c(1, 1, 1, 1))
plotDelaunay(
  del, col_edges = NULL, color = "salmon", col_borders = "black", asp = 1,
  axes = FALSE, xlab = NA, ylab = NA
)
par(opar)

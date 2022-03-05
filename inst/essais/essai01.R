library(RCDT)

x <- seq(0, 5, length.out = 30)
y <- seq(-5, 0, length.out = 30)
points <- as.matrix(expand.grid(x = x, y = y))
xy <- points[order(round(rowSums(points),6), points[, 2L]-points[, 1L]), ]

trgls <- getTriangles(xy)

library(rgl)
mesh <- tmesh3d(
  vertices = t(cbind(xy, 0)),
  indices = t(trgls) + 1L
)

wire3d(mesh)


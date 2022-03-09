library(RCDT)

points <- as.matrix(expand.grid(x = 1:4, y = 1:4))
storage.mode(points) <- "numeric"

x <- points[, 1L]
y <- points[, 2L]
o <- order(x+y, y-x)
xy <- cbind(x, y)[o, ]

del <- delaunay(xy)

plotDelaunay(del)

set.seed(666L)
xy <- xy[sample.int(nrow(xy)), ]
del <- delaunay(xy)

plotDelaunay(del)

####################
points <- cbind(
  c(1, 2, 1, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 4, 3, 4),
  c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 2, 3, 4, 3, 4, 4)
)
del <- delaunay(points)
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(
  del, xlab = NA, ylab = NA, axes = FALSE, asp = 1, lwd_edges = 2
)
par(opar)
# now we randomize the order of the points
set.seed(666L)
points2 <- points[sample.int(nrow(points)), ]
del2 <- delaunay(points2)
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(
  del2, xlab = NA, ylab = NA, axes = FALSE, asp = 1, lwd_edges = 2
)
par(opar)


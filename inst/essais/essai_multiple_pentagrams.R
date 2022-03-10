R <- sqrt((5-sqrt(5))/10)     # outer radius
r <- sqrt((25-11*sqrt(5))/10) # circumradius of the inner pentagon
k <- pi/180 # factor to convert degrees to radians
X <- R * vapply(0L:4L, function(i) cos(k * (90+72*i)), numeric(1L))
Y <- R * vapply(0L:4L, function(i) sin(k * (90+72*i)), numeric(1L))
x <- r * vapply(0L:4L, function(i) cos(k * (126+72*i)), numeric(1L))
y <- r * vapply(0L:4L, function(i) sin(k * (126+72*i)), numeric(1L))
vertices0 <- rbind(
  c(X[1L], Y[1L]),
  c(x[1L], y[1L]),
  c(X[2L], Y[2L]),
  c(x[2L], y[2L]),
  c(X[3L], Y[3L]),
  c(x[3L], y[3L]),
  c(X[4L], Y[4L]),
  c(x[4L], y[4L]),
  c(X[5L], Y[5L]),
  c(x[5L], y[5L])
)
# constraint edges: indices
edges1 <- cbind(1L:10L, c(2L:10L, 1L))

vertices1 <- vertices0 + 0.6
vertices2 <- vertices1; vertices2[, 1] <- vertices2[, 1] + 1.2
edges2 <- edges1 + 10L
vertices3 <- vertices1; vertices3[, 2] <- vertices3[, 2] + 1.2
edges3 <- edges2 + 10L
vertices4 <- vertices3; vertices4[, 1] <- vertices4[, 1] + 1.2
edges4 <- edges3 + 10L


vertices <- rbind(vertices1, vertices2, vertices3, vertices4)
edges <- rbind(edges1, edges2, edges3, edges4)

summary(vertices)

box <- rbind(
  c(0, 0),
  c(2.5, 0),
  c(2.5, 2.5),
  c(0, 2.5)
)

boxedges <- rbind(
  c(1, 2),
  c(2, 3),
  c(3, 4),
  c(4, 1)
) + max(edges)

vertices <- rbind(vertices, box)
edges <- rbind(edges, boxedges)

d <- delaunay(vertices, edges)
plotDelaunay(
  d, type = "n", axes = FALSE, xlab = NA, ylab = NA, asp = 1
)

library(microbenchmark)
microbenchmark(
  CDT = RCDT::delaunay(vertices, edges),
  # deldir = deldir::deldir(xy[,1], xy[,2], suppressMsge = TRUE),
  CGAL = RCGAL::delaunay(vertices, constraints = edges),
  # geometry = geometry::delaunayn(xy, options = "Pp"),
  # rgeos = rgeos::gDelaunayTriangulation(sp::SpatialPoints(xy)),
  # RTriangle = RTriangle::triangulate(RTriangle::pslg(xy)),
  # sf = sf::st_triangulate(sf::st_sfc(sf::st_multipoint(xy))),
  # tripack = tripack::tri.mesh(xy[,1], xy[,2]),
  times = 100
)

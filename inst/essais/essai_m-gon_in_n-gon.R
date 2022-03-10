library(RCDT)

n <- 100L
angles1 <- seq(0, 2*pi, length.out = n + 1L)[-1L]
outer_points <- cbind(cos(angles1), sin(angles1))
m <- 10L
angles2 <- seq(0, 2*pi, length.out = m + 1L)[-1L]
inner_points <- 0.5 * cbind(cos(angles2), sin(angles2))
points <- rbind(outer_points, inner_points)
# constraint edges
indices <- 1L:n
edges_outer <- cbind(
  indices, c(indices[-1L], indices[1L])
)
indices <- n + 1L:m
edges_inner <- cbind(
  indices, c(indices[-1L], indices[1L])
)
edges <- rbind(edges_outer, edges_inner)
# constrained Delaunay triangulation
del <- delaunay(points, edges) 
# plot
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(
  del, type = "n", asp = 1, lwd_borders = 3, col_borders = "black", 
  fillcolor = "random", luminosity = "dark", col_edges = "yellow",
  axes = FALSE, xlab = NA, ylab = NA
)
par(opar)
library(RCDT)

n <- 50L # number of sides of the outer polygon
angles1 <- head(seq(0, 2*pi, length.out = n + 1L), -1L)
outer_points <- cbind(cos(angles1), sin(angles1))
m <- 5L # number of sides of the inner polygon
angles2 <- head(seq(0, 2*pi, length.out = m + 1L), -1L)
phi <- (1+sqrt(5))/2 # the ratio  2-phi  will yield a perfect pentagram
inner_points <- (2-phi) * cbind(cos(angles2), sin(angles2))
points <- rbind(outer_points, inner_points)
# constraint edges
indices <- 1L:n
edges_outer <- cbind(indices, c(indices[-1L], indices[1L]))
indices <- n + 1L:m
edges_inner <- cbind(indices, c(indices[-1L], indices[1L]))
edges <- rbind(edges_outer, edges_inner)
# constrained Delaunay triangulation
del <- delaunay(points, edges) 
# there are 55 triangles:
del[["mesh"]]
# we make a cyclic palette of colors:
colors <- viridisLite::turbo(28)
colors <- c(colors, rev(colors[-1L]))
# plot
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(
  del, type = "n", asp = 1, lwd_borders = 3, col_borders = "black", 
  fillcolor = colors, col_edges = "black", lwd_edges = 1.5, 
  axes = FALSE, xlab = NA, ylab = NA
)
par(opar)
library(RCDT)

# a constrained Delaunay triangulation
innerSquare <- rbind( # (hole)
  c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
) # area: 4
outerSquare <- 2*innerSquare # area: 16
edges <- rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 1))
edges <- rbind(edges, edges + 4)
del <- delaunay(points = rbind(innerSquare, outerSquare), edges = edges)
delaunayArea(del) # 16-4

library(RCDT)

# points
nangles <- 12L
angles <- seq(0, 2*pi, length.out = nangles+1L)[-1L]
points <- cbind(cos(angles), sin(angles))
points <- rbind(points, points/1.5)

# constraint edges
indices <- 1L:nangles
edges <- cbind(
  indices,
  c(indices[-1L], indices[1L])
) 
edges <- rbind(edges, edges + nangles)

# Delaunay
del <- delaunay(points, edges) 

# plot
opar <- par(mar = c(1, 1, 1, 1))
plotDelaunay(del, color=FALSE, lwd_borders = 2, asp = 1, 
             axes = FALSE, xlab = NA, ylab = NA)
par(opar)



trgls <- del$triangles
bedges <- del$borderEdges

plot(points, pch = 19, asp = 1)
for(i in 1:nrow(trgls)){
  pts <- points[trgls[i, ], ]
  polygon(pts)
}
for(i in 1:nrow(bedges)){
  edge <- bedges[i, ]
  segments(
    x0 = points[edge[1L], 1L], y0 = points[edge[1L], 2L],
    x1 = points[edge[2L], 1L], y1 = points[edge[2L], 2L],
    col = "red", lwd = 2
  )
}



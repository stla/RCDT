library(RCDT)
library(Rvcg)
library(rgl)

cap <- vcgSphericalCap(angleRad = pi/2, subdivision = 3, normals = TRUE)
shade3d(cap, color = "green")
wire3d(cap)

R <- 1
h <- R*(1-sin(pi/2/2))
2*pi*R*h
vcgArea(cap)

points <- t(cap$vb[-4,])
del <- delaunay(points, elevation = TRUE)
del$surface


# volume
pi*h^2/3 * (3*R-h)
del$volume

vertices <- t(del$mesh$vb[-4, ])
border <- del$edges[del$edges[,"border"]==1, c(1,2)]
for(k in 1:nrow(border)){
  A <- vertices[border[k,1],]
  B <- vertices[border[k,2],]
  segments3d(rbind(A,B), color = "navy", lwd=4)
}
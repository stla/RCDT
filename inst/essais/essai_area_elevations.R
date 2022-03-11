library(RCDT)
library(Rvcg)
library(rgl)

cap <- vcgSphericalCap(angleRad = pi-pi/3, subdivision = 3, normals = TRUE)
shade3d(cap, color = "green")
wire3d(cap)

R <- 1
h <- R*(1-sin(pi/3/2))
2*pi*R*h
vcgArea(cap)

points <- t(cap$vb[-4,])
del <- delaunay(points, elevation = TRUE)
del$surface

nsims <- 3000
sims <- matrix(NA_real_, nrow=nsims, ncol = 3)
h <- R*(1-sin(pi/2/2))
for(i in 1:nsims){
  xy <- runif_in_sphere(1, 2, 1)
  k <- h * c(tcrossprod(xy))
  s <- sqrt(h * (2 - k))
  sims[i, ] <- c(s*xy[1,1], s*xy[1,2], 1-k)
}

sims[,3]<-sims[,3]-1+h
points <- sims


nsims <- 3000
sims <- matrix(NA_real_, nrow=nsims, ncol = 3)
R=2
h <- 1.3#R*(1-sin(pi/3/2))

xy <- runif_in_sphere(nsims, 2, 1)
k <- h * apply(xy, 1L, crossprod)
s <- sqrt(h * (2*R - k))
sims <- cbind(s*xy, R-k)


h
k = h*uniform_disc(&v);   // h(x^2+y^2)
s = sqrtf(h*(2.f-k));     // sqrt( h(2-h(x^2+y^2)) )
p->x = s*v.x;
p->y = s*v.y;
p->z = 1.f-k;
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
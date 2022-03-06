library(RCDT)

R <- sqrt((5-sqrt(5))/10)     # outer radius
r <- sqrt((25-11*sqrt(5))/10) # circumradius of the inner pentagon

# vertices
X <- R * vapply(0L:4L, function(i) cos(pi/180*(90+72*i)), numeric(1L))
Y <- R * vapply(0L:4L, function(i) sin(pi/180*(90+72*i)), numeric(1L))
x <- r * vapply(0L:4L, function(i) cos(pi/180*(126+72*i)), numeric(1L))
y <- r * vapply(0L:4L, function(i) sin(pi/180*(126+72*i)), numeric(1L))
vertices <- rbind(
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

# edge indices
edges <- cbind(1L:10L, c(2L:10L, 1L))

# Delaunay triangulation
del <- delaunay(vertices, edges)

# plot
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(del, asp = 1, color = "distinct", lwd_borders = 3,
             xlab = NA, ylab = NA, axes = FALSE)
par(opar)

# area
delaunayArea(del)
sqrt(650 - 290*sqrt(5)) / 4 # exact value


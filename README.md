The ‘RCDT’ package - constrained 2D Delaunay triangulation
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/RCDT/workflows/R-CMD-check/badge.svg)](https://github.com/stla/RCDT/actions)
<!-- badges: end -->

![](https://raw.githubusercontent.com/stla/RCDT/main/inst/images/CDT.png)

## The pentagram

``` r
# vertices
R <- sqrt((5-sqrt(5))/10)     # outer radius
r <- sqrt((25-11*sqrt(5))/10) # circumradius of the inner pentagon
X <- R * vapply(0L:4L, function(i) cos(pi/180 * (90+72*i)), numeric(1L))
Y <- R * vapply(0L:4L, function(i) sin(pi/180 * (90+72*i)), numeric(1L))
x <- r * vapply(0L:4L, function(i) cos(pi/180 * (126+72*i)), numeric(1L))
y <- r * vapply(0L:4L, function(i) sin(pi/180 * (126+72*i)), numeric(1L))
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
```

``` r
# edge indices
edges <- cbind(1L:10L, c(2L:10L, 1L))
```

``` r
# Delaunay triangulation
library(RCDT)
del <- delaunay(vertices, edges)
```

``` r
# plot
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(del, asp = 1, color = "distinct", lwd_borders = 3,
             xlab = NA, ylab = NA, axes = FALSE)
par(opar)
```

![](https://raw.githubusercontent.com/stla/RCDT/main/inst/images/pentagram.png)

``` r
# area
delaunayArea(del)
## [1] 0.3102707
sqrt(650 - 290*sqrt(5)) / 4 # exact value
## [1] 0.3102707
```

## An eight-pointed star

I found its vertices with the Julia library
[Luxor](http://juliagraphics.github.io/Luxor.jl/v0.10.3/index.html).

``` r
vertices <- rbind(
  c(2.121320343559643, 2.1213203435596424),
  c(0.5740251485476348, 1.38581929876693),
  c(0.0, 3.0),
  c(-0.5740251485476346, 1.38581929876693),
  c(-2.1213203435596424, 2.121320343559643),
  c(-1.38581929876693, 0.5740251485476349),
  c(-3.0, 0.0),
  c(-1.3858192987669302, -0.5740251485476345),
  c(-2.121320343559643, -2.1213203435596424),
  c(-0.5740251485476355, -1.3858192987669298),
  c(0.0, -3.0),
  c(0.574025148547635, -1.38581929876693),
  c(2.121320343559642, -2.121320343559643),
  c(1.3858192987669298, -0.5740251485476355),
  c(3.0, 0.0),
  c(1.38581929876693, 0.5740251485476349)
)
```

``` r
# edge indices
edges <- cbind(1L:16L, c(2L:16L, 1L))
```

``` r
library(RCDT)
del <- delaunay(vertices, edges)
```

``` r
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(
  del, asp = 1, color = "distinct", col_borders = "navy", lty_edges = 2, 
  lwd_borders = 3, lwd_edges = 2, xlab = NA, ylab = NA, axes = FALSE)
par(opar)
```

![](https://raw.githubusercontent.com/stla/RCDT/main/inst/images/eight-pointed_star.png)

## License

The ‘RCDT’ package as a whole is distributed under GPL-3 (GNU GENERAL
PUBLIC LICENSE version 3).

It uses the C++ library [CDT](https://github.com/artem-ogre/CDT) which is 
permissively licensed under MPL-2.0. A copy of the 'CDT' license is provided in 
the file **LICENSE.note**, and the source code of this library can be found in 
the **src** folder.

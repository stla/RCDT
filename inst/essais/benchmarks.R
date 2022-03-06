library(microbenchmark)

n <- 10000
xy <- cbind(runif(n), runif(n))

microbenchmark(
  RCDT = RCDT::delaunay(xy),
  # deldir = deldir::deldir(xy[,1], xy[,2], suppressMsge = TRUE),
  # CGAL = RCGAL::delaunay(xy),
  geometry = geometry::delaunayn(xy, options = "Pp"),
  # rgeos = rgeos::gDelaunayTriangulation(sp::SpatialPoints(xy)),
  RTriangle = RTriangle::triangulate(RTriangle::pslg(xy)),
  # sf = sf::st_triangulate(sf::st_sfc(sf::st_multipoint(xy))),
  tripack = tripack::tri.mesh(xy[,1], xy[,2]),
  times = 10
)
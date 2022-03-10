#' @title 2D Delaunay triangulation
#' @description Performs a (constrained) Delaunay triangulation of a set of 
#'   2d points.
#'
#' @param points a numeric matrix with two or three columns (three colums for 
#'   an elevated Delaunay triangulation)
#' @param edges the edges for the constrained Delaunay triangulation, 
#'   an integer matrix with two columns; \code{NULL} for no constraint
#' @param elevation Boolean, whether to perform an elevate Delaunay 
#'   triangulation (also known as 2.5D Delaunay triangulation)
#'
#' @return A list. There are three possibilities.
#' #' \itemize{
#'   \item \strong{If the dimension is 2} and \code{edges=NULL},
#'         the returned value is a list with two fields:
#'         \code{mesh} and \code{edges}. 
#'         The \code{mesh} field is an object of
#'         class \code{\link[rgl]{mesh3d}}, ready for plotting with the 
#'         \strong{rgl} package. 
#'         The \code{edges} field provides the indices of the vertices of the 
#'         edges, given by the first two columns of a three-columns integer 
#'         matrix. 
#'         The third column, named \code{border}, only contains some
#'         zeros and some ones; a border (exterior) edge is labelled by a
#'         \code{1}. 
#'   \item \strong{If the dimension is 2} and \code{edges} is not
#'         \code{NULL}, the returned value is a list with
#'         three fields: \code{mesh}, \code{edges}, and \code{constraints}. 
#'         The \code{mesh} and \code{edges} fields are similar to the previous 
#'         case, the unconstrained Delaunay triangulation. 
#'         The \code{constraints} field is an integer matrix with
#'         two columns, it represents the constraint edges. Note that in 
#'         general these are the same edges as the ones provided by the user,
#'         but not always, in some rare corner cases.
#'   \item \strong{If} \code{elevation=TRUE}, the returned value is a list with
#'         five fields: \code{mesh}, \code{edges}, 
#'         and \code{surface}. The \code{mesh} field is an object of
#'         class \code{\link[rgl]{mesh3d}}, ready for plotting with the 
#'         \strong{rgl} package. The \code{edges} field is similar to the 
#'         previous cases. 
#'         The \code{volume} field provides a number, the sum of the volumes 
#'         under the Delaunay triangles, that is to say the total volume
#'         under the triangulated surface. Finally, the \code{surface} field
#'         provides the sum of the areas of all triangles, thereby
#'         approximating the area of the triangulated surface.
#' }
#'  
#' @note The triangulation can depend on the order of the points; this is 
#'   shown in the examples.
#'
#' @importFrom rgl tmesh3d
#' @importFrom Rvcg vcgGetEdge
#' 
#' @export
#'
#' @examples library(RCDT)
#' # random points in a square ####
#' set.seed(314)
#' library(uniformly)
#' square <- rbind(
#'   c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
#' )
#' pts_in_square <- runif_in_cube(10L, d = 2L)
#' pts <- rbind(square, pts_in_square)
#' del <- delaunay(pts)
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   del, type = "n", xlab = NA, ylab = NA, asp = 1, 
#'   fillcolor = "random", luminosity = "light", lty_edges = "dashed"
#' )
#' par(opar)
#' 
#' # the order of the points matters ####
#' # the Delaunay triangulation is not unique in general; 
#' #   it can depend on the order of the points
#' points <- cbind(
#'   c(1, 2, 1, 3, 2, 1, 4, 3, 2, 1, 4, 3, 2, 4, 3, 4),
#'   c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 2, 3, 4, 3, 4, 4)
#' )
#' del <- delaunay(points)
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   del, type = "p", pch = 19, xlab = NA, ylab = NA, axes = FALSE, 
#'   asp = 1, lwd_edges = 2, lwd_borders = 3
#' )
#' par(opar)
#' # now we randomize the order of the points
#' set.seed(666L)
#' points2 <- points[sample.int(nrow(points)), ]
#' del2 <- delaunay(points2)
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   del2, type = "p", pch = 19, xlab = NA, ylab = NA, axes = FALSE, 
#'   asp = 1, lwd_edges = 2, lwd_borders = 3
#' )
#' par(opar)
#' 
#' # a constrained Delaunay triangulation: outer and inner dodecagons ####
#' # points
#' nsides <- 12L
#' angles <- seq(0, 2*pi, length.out = nsides+1L)[-1L]
#' points <- cbind(cos(angles), sin(angles))
#' points <- rbind(points, points/1.5)
#' # constraint edges
#' indices <- 1L:nsides
#' edges_outer <- cbind(
#'   indices, c(indices[-1L], indices[1L])
#' )
#' edges_inner <- edges_outer + nsides
#' edges <- rbind(edges_outer, edges_inner)
#' # constrained Delaunay triangulation
#' del <- delaunay(points, edges) 
#' # plot
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   del, type = "n", fillcolor = "yellow", lwd_borders = 2, asp = 1, 
#'   axes = FALSE, xlab = NA, ylab = NA
#' )
#' par(opar)
#' 
#' # another constrained Delaunay triangulation: a face ####
#' V <- read.table(
#'   system.file("extdata", "face_vertices.txt", package = "RCDT")
#' )
#' E <- read.table(
#'   system.file("extdata", "face_edges.txt", package = "RCDT")
#' )
#' del <- delaunay(
#'   points = as.matrix(V)[, c(2L, 3L)], edges = as.matrix(E)[, c(2L, 3L)]
#' )
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   del, type="n", col_edges = NULL, fillcolor = "salmon", 
#'   col_borders = "black", col_constraints = "purple", 
#'   lwd_borers = 3, lwd_constraints = 3,
#'   asp = 1, axes = FALSE, xlab = NA, ylab = NA
#' )
#' par(opar)
delaunay <- function(points, edges = NULL, elevation = FALSE){
  stopifnot(isBoolean(elevation))
  if(!is.matrix(points) || !is.numeric(points)){
    stop(
      "The `points` argument must be a numeric matrix with two or three columns.", 
      call. = TRUE
    )
  }
  dimension <- ncol(points)
  if(!is.element(dimension, c(2L, 3L))){
    stop(
      "The `points` argument must be a numeric matrix with two or three columns.", 
      call. = TRUE
    )
  }
  if(any(is.na(points))){
    stop("Missing values are not allowed.", call. = TRUE)
  }
  if(elevation){
    if(dimension != 3L){
      stop(
        "To get an elevated Delaunay tessellation (`elevation=TRUE`), ",
        "you have to provide three-dimensional points.",
        call. = TRUE
      )
    }
    x <- points[, 1L]
    y <- points[, 2L]
    x <- (x - min(x)) / diff(range(x))
    y <- (y - min(y)) / diff(range(y))
    o <- order(round(x+y, 6L), y-x)
    xy <- cbind(x, y)[o, ]
    if(anyDuplicated(xy)){
      stop("There are some duplicated points.", call. = TRUE)
    }
    cpp <- Rcpp_delaunay(xy)
    Triangles <- cpp[["triangles"]]
    vertices <- points[o, ]
    mesh <- tmesh3d(
      vertices = t(vertices),
      indices = t(Triangles),
      homogeneous = FALSE
    )
    volumes_and_areas <- apply(Triangles, 1L, function(trgl){
      trgl <- vertices[trgl, ]
      c(
        volume_under_triangle(trgl[, 1L], trgl[, 2L], trgl[, 3L]),
        triangleArea(trgl[1L, ], trgl[2L, ], trgl[3L, ])
      )
    })
    out <- list(
      "mesh"    = mesh,
      "edges"   = `colnames<-`(
        as.matrix(vcgGetEdge(mesh))[, -3L], c("v1", "v2", "border")
      ),
      "volume"  = sum(volumes_and_areas[1L, ]),
      "surface" = sum(volumes_and_areas[2L, ])
    )
    attr(out, "elevation") <- TRUE
    class(out) <- "delaunay"
    return(out)
  }
  if(anyDuplicated(points)){
    stop("There are some duplicated points.", call. = TRUE)
  }
  storage.mode(points) <- "double"
  if(is.null(edges)){
    cpp <- Rcpp_delaunay(points)
    mesh <- tmesh3d(
      vertices = rbind(t(points), 0),
      indices = t(cpp[["triangles"]])
    )
    Edges <- `colnames<-`(
      as.matrix(vcgGetEdge(mesh))[, c(1L, 2L, 4L)], c("v1", "v2", "border")
    )
    out <- list(
      "mesh"  = mesh,
      "edges" = Edges
    )
  }else{
    if(!is.matrix(edges) || !is.numeric(edges) || ncol(edges) != 2L){
      stop(
        "The `edges` argument must be an integer matrix with two columns.", 
        call. = TRUE
      )
    }
    if(any(is.na(edges))){
      stop("Missing values in `edges` are not allowed.", call. = TRUE)
    }
    storage.mode(edges) <- "integer"
    stopifnot(all(edges >= 1L))
    stopifnot(all(edges <= nrow(points)))
    edges <- t(apply(edges, 1L, sort))
    if(anyDuplicated(edges)){
      stop("There are some duplicated constraint edges.", call. = TRUE)
    }
    if(any(edges[, 1L] == edges[, 2L])){
      stop("There are some invalid constraint edges.", call. = TRUE)
    }
    cpp <- Rcpp_constrained_delaunay(points, edges)
    mesh <- tmesh3d(
      vertices = rbind(t(points), 0),
      indices = t(cpp[["triangles"]])
    )
    Edges <- `colnames<-`(
      as.matrix(vcgGetEdge(mesh))[, c(1L, 2L, 4L)], c("v1", "v2", "border")
    )
    out <- list(
      "mesh"        = mesh,
      "edges"       = Edges,
      "constraints" = cpp[["borderEdges"]]
    )
    attr(out, "constrained") <- TRUE
  }
  attr(out, "vertices") <- points
  class(out) <- "delaunay"
  out
}

#' @title Area of Delaunay triangulation
#' @description Computes the area of a region subject to Delaunay triangulation.
#'
#' @param del an output of \code{\link{delaunay}} executd with 
#'   \code{elevation=FALSE}
#'
#' @return A number, the area of the region triangulated by the Delaunay 
#'   triangulation.
#' @export
#'
#' @examples library(RCDT)
#' # random points in a square ####
#' set.seed(666L)
#' library(uniformly)
#' square <- rbind(
#'   c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
#' )
#' pts <- rbind(square, runif_in_cube(8L, d = 2L))
#' del <- delaunay(pts)
#' delaunayArea(del)
#' 
#' # a constrained Delaunay triangulation: outer and inner squares ####
#' innerSquare <- rbind( # the hole
#'   c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
#' ) # area: 4
#' outerSquare <- 2*innerSquare # area: 16
#' points <- rbind(innerSquare, outerSquare)
#' edges_inner <- rbind(c(1L, 2L), c(2L, 3L), c(3L, 4L), c(4L, 1L))
#' edges_outer <- edges_inner + 4L
#' edges <- rbind(edges_inner, edges_outer)
#' del <- delaunay(points, edges = edges)
#' delaunayArea(del) # 16-4
delaunayArea <- function(del){
  stopifnot(inherits(del, "delaunay"))
  if(isTRUE(attr(del, "elevation"))){
    stop(
      "This function is not conceived for elevated Delaunay triangulations.",
      call. = TRUE
    )
  }
  triangles <- del[["mesh"]][["it"]]
  vertices <- attr(del, "vertices")
  ntriangles <- ncol(triangles)
  areas <- numeric(ntriangles)
  for(i in 1L:ntriangles){
    points <- vertices[triangles[, i], ]
    areas[i] <- triangleArea(points[1L, ], points[2L, ], points[3L, ])
  }
  sum(areas)
}
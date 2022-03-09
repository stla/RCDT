#' @title 2D Delaunay triangulation
#' @description Performs a (constrained) Delaunay triangulation of a set of 
#'   2d points.
#'
#' @param points a numeric matrix with two columns
#' @param edges the edges for the constrained Delaunay triangulation, 
#'   an integer matrix with two columns; \code{NULL} for no constraint
#'
#' @return A list. It has two fields for an unconstrained Delaunay 
#'  triangulation: \strong{triangles}, an integer matrix with three columns, 
#'  it provides the indices of the vertices of the Delaunay triangles, and a 
#'  field \strong{allEdges}, which is an integer matrix with two columns, 
#'  providing the indices of all the edges of the triangulation. For a 
#'  constrained Delaunay triangulation, there is an additional field 
#'  \strong{borderEdges}, an integer matrix with two columns, providing the 
#'  indices of the edges given as constraints.
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
delaunay <- function(points, edges = NULL){
  if(!is.matrix(points) || !is.numeric(points) || ncol(points) != 2L){
    stop(
      "The `points` argument must be a numeric matrix with two columns.", 
      call. = TRUE
    )
  }
  if(any(is.na(points))){
    stop("Missing values are not allowed.", call. = TRUE)
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
      "edges" = Edges,
      "area"  = 9999
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
      "constraints" = cpp[["borderEdges"]],
      "area"        = 9999
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
#' @param del an output of \code{\link{delaunay}}
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
  triangles <- del[["triangles"]]
  vertices <- attr(del, "vertices")
  ntriangles <- nrow(triangles)
  areas <- numeric(ntriangles)
  for(i in 1L:ntriangles){
    points <- vertices[triangles[i, ], ]
    areas[i] <- triangleArea(points[1L, ], points[2L, ], points[3L, ])
  }
  sum(areas)
}
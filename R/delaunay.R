#' @title 2D Delaunay triangulation
#' @description Performs a (constrained) Delaunay triangulation of a set of 
#'   2d points.
#'
#' @param points a numeric matrix with two columns
#' @param edges the edges for the constrained Delaunay triangulation, 
#'   an integer matrix with two columns; \code{NULL} for no constraint
#'
#' @return A list. It has two fields for an unconstrained Delaunay trianguation: 
#'  \strong{triangles}, an integer matrix with three columns, it provides 
#'  the indices of the vertices of the Delaunay triangles, and a field 
#'  \strong{allEdges}, which is an integer matrix with two columns, providing 
#'  the indices of all the edges of the triangulation. For a constrained 
#'  Delaunay tessellation, there is an additional field \strong{borderEdges}, 
#'  an integer matrix with two columns, providing the indices of the edges 
#'  given as constraints.
#'
#' @export
#'
#' @examples # random points in a square
#' set.seed(314)
#' library(RCDT)
#' library(uniformly)
#' square <- rbind(
#'   c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
#' )
#' ptsin <- runif_in_cube(10L, d = 2L)
#' pts <- rbind(square, ptsin)
#' del <- delaunay(pts)
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   del, xlab = NA, ylab = NA, asp = 1, color = "random", luminosity = "dark"
#' )
#' par(opar)
#' 
#' # a constrained Delaunay triangulation ####
#' # points
#' nangles <- 12L
#' angles <- seq(0, 2*pi, length.out = nangles+1L)[-1L]
#' points <- cbind(cos(angles), sin(angles))
#' points <- rbind(points, points/1.5)
#' 
#' # constraint edges
#' indices <- 1L:nangles
#' edges <- cbind(
#'   indices,
#'   c(indices[-1L], indices[1L])
#' ) 
#' edges <- rbind(edges, edges + nangles)
#' 
#' # Delaunay
#' del <- delaunay(points, edges) 
#' 
#' # plot
#' opar <- par(mar = c(1, 1, 1, 1))
#' plotDelaunay(del, color = "yellow", lwd_borders = 2, asp = 1, 
#'              axes = FALSE, xlab = NA, ylab = NA)
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
#' opar <- par(mar = c(1, 1, 1, 1))
#' plotDelaunay(
#'   del, col_edges = NULL, color = "salmon", col_borders = "black", asp = 1,
#'   axes = FALSE, xlab = NA, ylab = NA
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
  if(is.null(edges)){
    out <- Rcpp_delaunay(points)
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
      stop("There are some duplicated edges.", call. = TRUE)
    }
    if(any(edges[, 1L] == edges[, 2L])){
      stop("There are some invalid edges.", call. = TRUE)
    }
    out <- Rcpp_constrained_delaunay(points, edges)
    attr(out, "constrained") <- TRUE
  }
  attr(out, "vertices") <- points
  class(out) <- "delaunay"
  out
}

#' @title Area of Delaunay trianguation
#' @description Computes the area of a Delaunay triangulation.
#'
#' @param del an output of \code{\link{delaunay}}
#'
#' @return A number, the area of the Delaunay triangulation.
#' @export
#'
#' @examples library(RCDT)
#' set.seed(666)
#' library(uniformly)
#' square <- rbind(
#'   c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
#' )
#' pts <- rbind(square, runif_in_cube(8L, d = 2L))
#' del <- delaunay(pts)
#' delaunayArea(del)
#' 
#' # a constrained Delaunay triangulation ####
#' innerSquare <- rbind( # the hole
#'   c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
#' ) # area: 4
#' outerSquare <- 2*innerSquare # area: 16
#' edges <- rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 1))
#' edges <- rbind(edges, edges + 4)
#' del <- delaunay(points = rbind(innerSquare, outerSquare), edges = edges)
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
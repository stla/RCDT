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
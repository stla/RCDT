makeTriangle <- function(vertices, indices){
  vertices[indices, ]
}

#' @title Plot 2D Delaunay tessellation
#' @description Plot a 2D Delaunay tessellation.
#'
#' @param del an output of \code{\link{delaunay}} without constraints 
#'   (\code{edges=NULL})
#' @param col_edges the color of the edges of the triangles; \code{NULL} for
#'   no edges
#' @param col_borders the color of the border edges for a constrained 
#'   Delaunay tessellation; \code{NULL} for no border edges
#' @param color controls the filling colors of the triangles, either
#'   \code{FALSE} for no color, a single color, \code{"random"} to get multiple 
#'   colors with \code{\link[randomcoloR]{randomColor}}, or \code{"distinct"} 
#'   get multiple colors with \code{\link[randomcoloR]{distinctColorPalette}}
#' @param hue,luminosity if \code{color = "random"}, these arguments are passed
#'   to \code{\link[randomcoloR]{randomColor}}
#' @param lty_edges,lwd_edges graphical parameters for the edges of the triangles
#' @param lty_borders,lwd_borders graphical parameters for the border edges in 
#'   the case of a constrained Delaunay tessellation
#' @param ... arguments passed to \code{\link{plot}} for the vertices
#'
#' @return No value, just renders a 2D plot.
#' @export
#' @importFrom randomcoloR randomColor distinctColorPalette
#' @importFrom graphics plot polygon par segments
#' @importFrom gplots col2hex
#'
#' @examples # random points in a square
#' set.seed(314)
#' library(tessellation)
#' library(uniformly)
#' square <- rbind(
#'   c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
#' )
#' ptsin <- runif_in_cube(10L, d = 2L)
#' pts <- rbind(square, ptsin)
#' d <- delaunay(pts)
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   d, xlab = NA, ylab = NA, asp = 1, color = "random", luminosity = "dark"
#' )
#' par(opar)
plotDelaunay <- function(
  del, col_edges = "black", col_borders = "red", color = "distinct", 
  hue = "random", luminosity = "light", 
  lty_edges = par("lty"), lwd_edges = par("lwd"), 
  lty_borders = par("lty"), lwd_borders = par("lwd"), ...
){
  if(!inherits(del, "delaunay")){
    stop(
      "The argument `del` must be an output of the `delaunay` function.",
      call. = TRUE
    )
  }
  # if(isTRUE(attr(tessellation, "elevation"))){
  #   stop(
  #     "This function is not conceived for elevated Delaunay tessellations.",
  #     call. = TRUE
  #   )
  # }
  vertices <- attr(del, "vertices")
  plot(vertices, type = "n", ...)
  if(!isFALSE(color)){
    color <- tryCatch({
      col2hex(color)
    }, error = function(e){
      match.arg(color, c("random", "distinct"))
    })
    triangles <- del[["triangles"]]
    ntriangles <- nrow(triangles)
    if(color == "random"){
      colors <- randomColor(ntriangles, hue = hue, luminosity = luminosity)
    }else if(color == "distinct"){
      colors <- distinctColorPalette(ntriangles)
    }else{
      colors <- rep(color, ntriangles)
    }
    for(i in 1L:ntriangles){
      triangle <- makeTriangle(vertices, triangles[i, ])
      polygon(triangle, border = NA, col = colors[i])
    }
  }
  if(!is.null(col_edges)){
    edges <- del[["allEdges"]]
    for(i in 1L:nrow(edges)){
      edge <- edges[i, ]
      p0 <- vertices[edge[1L], ]
      p1 <- vertices[edge[2L], ]
      segments(
        p0[1L], p0[2L], p1[1L], p1[2L], col = col_edges, 
        lty = lty_edges, lwd = lwd_edges
      )
    }
  }
  if(isTRUE(attr(del, "constrained"))){
    edges <- del[["borderEdges"]]
    for(i in 1L:nrow(edges)){
      edge <- edges[i, ]
      p0 <- vertices[edge[1L], ]
      p1 <- vertices[edge[2L], ]
      segments(
        p0[1L], p0[2L], p1[1L], p1[2L], col = col_borders, 
        lty = lty_borders, lwd = lwd_borders
      )
    }
  }
  invisible(NULL)
}

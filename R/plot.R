makeTriangle <- function(vertices, indices){
  vertices[indices, ]
}

#' @title Plot 2D Delaunay tessellation
#' @description Plot a 2D Delaunay tessellation.
#'
#' @param del an output of \code{\link{delaunay}} without constraints 
#'   (\code{edges=NULL})
#' @param border the color of the borders of the triangles; \code{NULL} for
#'   no borders
#' @param color controls the filling colors of the triangles, either
#'   \code{FALSE} for no color, \code{"random"} to use
#'   \code{\link[randomcoloR]{randomColor}}, or \code{"distinct"} to use
#'   \code{\link[randomcoloR]{distinctColorPalette}}
#' @param hue,luminosity if \code{color = "random"}, these arguments are passed
#'   to \code{\link[randomcoloR]{randomColor}}
#' @param lty,lwd graphical parameters
#' @param ... arguments passed to \code{\link{plot}}
#'
#' @return No value, just renders a 2D plot.
#' @export
#' @importFrom randomcoloR randomColor distinctColorPalette
#' @importFrom graphics plot polygon par segments
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
  del, border = "black", color = "distinct", hue = "random",
  luminosity = "light", lty = par("lty"), lwd = par("lwd"), ...
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
    color <- match.arg(color, c("random", "distinct"))
    triangles <- del[["triangles"]]
    ntriangles <- nrow(triangles)
    if(color == "random"){
      colors <- randomColor(ntriangles, hue = hue, luminosity = luminosity)
    }else{
      colors <- distinctColorPalette(ntriangles)
    }
    for(i in 1L:ntriangles){
      triangle <- makeTriangle(vertices, triangles[i, ])
      polygon(triangle, border = NA, col = colors[i])
    }
  }
  if(!is.null(border)){
    edges <- del[["allEdges"]]
    for(i in 1:nrow(edges)){
      edge <- edges[i, ]
      p0 <- vertices[edge[1L], ]
      p1 <- vertices[edge[2L], ]
      segments(
        p0[1L], p0[2L], p1[1L], p1[2L], col = border, lty = lty, lwd = lwd
      )
    }
  }
}

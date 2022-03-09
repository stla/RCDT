#' @title Plot 2D Delaunay triangulation
#' @description Plot a constrained or unconstrained 2D Delaunay triangulation.
#'
#' @param del an output of \code{\link{delaunay}} without constraints 
#'   (\code{edges=NULL}) or with constraints
#' @param col_edges the color of the edges of the triangles which are not
#'   border edges nor constraint edges; \code{NULL} for no color
#' @param col_borders the color of the border edges; note that the border
#'   edges can contain the constraint edges for a constrained
#'   Delaunay triangulation; \code{NULL} for no color
#' @param col_constraints for a constrained Delaunay triangulation, the color
#'   of the constraint edges which are not border edges;
#'   \code{NULL} for no color
#' @param fillcolor controls the filling colors of the triangles, either
#'   \code{NULL} for no color, a single color, \code{"random"} to get multiple
#'   colors with \code{\link[randomcoloR]{randomColor}}, or \code{"distinct"}
#'   get multiple colors with \code{\link[randomcoloR]{distinctColorPalette}}
#' @param hue,luminosity if \code{color = "random"}, these arguments are passed
#'   to \code{\link[randomcoloR]{randomColor}}
#' @param lty_edges,lwd_edges graphical parameters for the edges which are not
#'   border edges nor constraint edges
#' @param lty_borders,lwd_borders graphical parameters for the border edges
#' @param lty_constraints,lwd_constraints in the case of a constrained Delaunay
#'   triangulation, graphical parameters for the constraint edges which are
#'   not border edges
#' @param ... arguments passed to \code{\link{plot}} for the vertices, such as
#'   \code{type="n"}, \code{asp=1}, \code{axes=FALSE}, etc
#'
#' @return No value, just renders a 2D plot.
#'
#' @seealso The \code{mesh} field in the output of \code{\link{delaunay}} 
#'   for an interactive plot.
#'
#' @export
#' @importFrom randomcoloR randomColor distinctColorPalette
#' @importFrom graphics plot polygon par segments
#' @importFrom gplots col2hex
#'
#' @examples library(RCDT)
#' # random points in a square
#' square <- rbind(
#'   c(-1, 1), c(1, 1), c(1, -1), c(-1, -1)
#' )
#' library(uniformly)
#' set.seed(314)
#' pts_in_square <- runif_in_cube(10L, d = 2L)
#' pts <- rbind(square, pts_in_square)
#' d <- delaunay(pts)
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   d, type = "n", xlab = NA, ylab = NA, axes = FALSE, asp = 1, 
#'   fillcolor = "random", luminosity = "dark", lwd_borders = 3
#' )
#' par(opar)
#' 
#' # a constrained Delaunay triangulation: pentagram ####
#' # vertices
#' R <- sqrt((5-sqrt(5))/10)     # outer circumradius
#' r <- sqrt((25-11*sqrt(5))/10) # circumradius of the inner pentagon
#' k <- pi/180 # factor to convert degrees to radians
#' X <- R * vapply(0L:4L, function(i) cos(k * (90+72*i)), numeric(1L))
#' Y <- R * vapply(0L:4L, function(i) sin(k * (90+72*i)), numeric(1L))
#' x <- r * vapply(0L:4L, function(i) cos(k * (126+72*i)), numeric(1L))
#' y <- r * vapply(0L:4L, function(i) sin(k * (126+72*i)), numeric(1L))
#' vertices <- rbind(
#'   c(X[1L], Y[1L]),
#'   c(x[1L], y[1L]),
#'   c(X[2L], Y[2L]),
#'   c(x[2L], y[2L]),
#'   c(X[3L], Y[3L]),
#'   c(x[3L], y[3L]),
#'   c(X[4L], Y[4L]),
#'   c(x[4L], y[4L]),
#'   c(X[5L], Y[5L]),
#'   c(x[5L], y[5L])
#' )
#' # constraint edge indices (= boundary)
#' edges <- cbind(1L:10L, c(2L:10L, 1L))
#' # constrained Delaunay triangulation
#' del <- delaunay(vertices, edges)
#' # plot
#' opar <- par(mar = c(0, 0, 0, 0))
#' plotDelaunay(
#'   del, type = "n", asp = 1, fillcolor = "distinct", lwd_borders = 3,
#'   xlab = NA, ylab = NA, axes = FALSE
#' )
#' par(opar)
#' # interactive plot with 'rgl'
#' mesh <- del[["mesh"]]
#' library(rgl)
#' open3d(windowRect = c(100, 100, 612, 612))
#' shade3d(mesh, color = "red", specular = "orangered")
#' wire3d(mesh, color = "black", lwd = 3, specular = "black")
#' # plot only the border edges - we could find them in `del[["edges"]]` but 
#'   # we will use the 'rgl' function `getBoundary3d`
#' open3d(windowRect = c(100, 100, 612, 612))
#' shade3d(mesh, color = "darkred", specular = "firebrick")
#' shade3d(getBoundary3d(mesh), lwd = 3)
plotDelaunay <- function(
  del, 
  col_edges = "black", col_borders = "red", col_constraints = "green",
  fillcolor = "distinct", hue = "random", luminosity = "light",
  lty_edges = par("lty"), lwd_edges = par("lwd"),
  lty_borders = par("lty"), lwd_borders = par("lwd"),
  lty_constraints = par("lty"), lwd_constraints = par("lwd"), ...
){
  if(!inherits(del, "delaunay")){
    stop(
      "The argument `del` must be an output of the `delaunay` function.",
      call. = TRUE
    )
  }
  if(isTRUE(attr(del, "elevation"))){
    stop(
      "This function is not conceived for elevated Delaunay triangulations.",
      call. = TRUE
    )
  }
  vertices <- attr(del, "vertices")
  if(ncol(vertices) != 2L){
    stop(
      sprintf("Invalid dimension (%d instead of 2).", ncol(vertices)),
      call. = TRUE
    )
  }
  plot(vertices, ...)
  if(!isFalsy(fillcolor)){
    fillcolor <- tryCatch({
      col2hex(fillcolor)
    }, error = function(e){
      match.arg(fillcolor, c("random", "distinct"))
    })
    triangles <- t(del[["mesh"]][["it"]])
    ntriangles <- nrow(triangles)
    if(fillcolor == "random"){
      colors <- randomColor(ntriangles, hue = hue, luminosity = luminosity)
    }else if(fillcolor == "distinct"){
      colors <- distinctColorPalette(ntriangles)
    }else{
      colors <- rep(fillcolor, ntriangles)
    }
    for(i in 1L:ntriangles){
      triangle <- makeTriangle(vertices, triangles[i, ])
      polygon(triangle, border = NA, col = colors[i])
    }
  }
  constraintEdges <- del[["constraints"]]
  allEdges <- del[["edges"]]
  borderEdges <- allEdges[allEdges[, "border"] == 1L, c(1L, 2L)]
  allEdges <- allEdges[, c(1L, 2L)]
  specialEdges <- unionEdges(borderEdges, constraintEdges)
  constraintEdges <- subtractEdges(specialEdges, borderEdges)
  otherEdges <- subtractEdges(allEdges, specialEdges)
  if(!isFalsy(col_edges)){
    edges <- otherEdges
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
  if(!isFalsy(col_borders)){
    edges <- borderEdges
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
  if(!is.null(constraintEdges) && !isFalsy(col_constraints)){
    edges <- constraintEdges
    for(i in 1L:nrow(edges)){
      edge <- edges[i, ]
      p0 <- vertices[edge[1L], ]
      p1 <- vertices[edge[2L], ]
      segments(
        p0[1L], p0[2L], p1[1L], p1[2L], col = col_constraints,
        lty = lty_constraints, lwd = lwd_constraints
      )
    }
  }
  invisible(NULL)
}

distance <- function(A, B){
  sqrt(c(crossprod(A-B)))
}

triangleArea <- function(A, B, C){
  a <- distance(B, C)
  b <- distance(A, C)
  c <- distance(A, B)
  s <- (a + b + c) / 2
  sqrt(s*(s-a)*(s-b)*(s-c))
}

volume_under_triangle <- function(x, y, z){
  sum(z) *
    (x[1L]*y[2L] - x[2L]*y[1L] + x[2L]*y[3L] -
       x[3L]*y[2L] + x[3L]*y[1L] - x[1L]*y[3L]) / 6
}

isFalsy <- function(x){
  isFALSE(x) || is.null(x) || is.na(x)
}

makeTriangle <- function(vertices, indices){
  vertices[indices, ]
}

incenter <- function(triangle){
  A <- triangle[1L, ]
  B <- triangle[3L, ]
  C <- triangle[3L, ]
  a <- distance(B, C)
  b <- distance(A, C)
  c <- distance(A, B)
  (a*A + b*B + c*C) / (a + b + c)
}

subtractEdges <- function(Edges, edges){
  if(is.null(edges)){
    return(Edges)
  }
  if(nrow(Edges) == nrow(edges)){
    return(NULL)
  }
  Strings <- paste0(Edges[, 1L], "-", Edges[, 2L])
  strings <- paste0(edges[, 1L], "-", edges[, 2L])
  rownames(Edges) <- Strings
  Edges[setdiff(Strings, strings), ]
}

unionEdges <- function(edges1, edges2){
  if(is.null(edges2)){
    return(edges1)
  }
  Edges <- rbind(edges1, edges2)
  Edges[!duplicated(Edges), ]
}

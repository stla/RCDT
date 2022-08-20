nsides <- 12L
angles <- seq(0, 2*pi, length.out = nsides+1L)[-1L]
points <- cbind(cos(angles), sin(angles))
points <- rbind(points, points/1.5)
# constraint edges
indices <- 1L:nsides
edges_outer <- cbind(
  indices, c(indices[-1L], indices[1L])
)
edges_inner <- edges_outer + nsides
edges <- rbind(edges_outer, edges_inner)

tedges <- matrix(
  c(
    1L, 2L, 1L, 
    1L, 12L, 1L, 
    1L, 13L, 0L, 
    2L, 3L, 1L, 
    2L, 13L, 0L, 
    2L, 14L, 0L, 
    3L, 4L, 1L, 
    3L, 14L, 0L, 
    3L, 15L, 0L, 
    3L, 16L, 0L, 
    4L, 5L, 1L, 
    4L, 16L, 0L, 
    4L, 17L, 0L, 
    5L, 6L, 1L, 
    5L, 17L, 0L, 
    6L, 7L, 1L, 
    6L, 17L, 0L, 
    6L, 18L, 0L, 
    7L, 8L, 1L, 
    7L, 18L, 0L, 
    7L, 19L, 0L, 
    7L, 20L, 0L, 
    8L, 9L, 1L, 
    8L, 20L, 0L, 
    9L, 10L, 1L, 
    9L, 20L, 0L, 
    9L, 21L, 0L, 
    9L, 22L, 0L, 
    10L, 11L, 1L, 
    10L, 22L, 0L, 
    10L, 23L, 0L, 
    11L, 12L, 1L, 
    11L, 23L, 0L, 
    12L, 13L, 0L, 
    12L, 23L, 0L, 
    12L, 24L, 0L, 
    13L, 14L, 1L, 
    13L, 24L, 1L, 
    14L, 15L, 1L, 
    15L, 16L, 1L, 
    16L, 17L, 1L, 
    17L, 18L, 1L, 
    18L, 19L, 1L, 
    19L, 20L, 1L, 
    20L, 21L, 1L, 
    21L, 22L, 1L, 
    22L, 23L, 1L, 
    23L, 24L, 1L
  ),
  nrow = 48L, ncol = 3L, byrow = TRUE
)

tedges <- matrix(
  c(
    1L, 2L, 1L, 
    1L, 12L, 1L, 
    1L, 13L, 0L, 
    2L, 3L, 1L, 
    2L, 13L, 0L, 
    2L, 14L, 0L, 
    3L, 4L, 1L, 
    3L, 14L, 0L, 
    3L, 15L, 0L, 
    3L, 16L, 0L, 
    4L, 5L, 1L, 
    4L, 16L, 0L, 
    4L, 17L, 0L, 
    5L, 6L, 1L, 
    5L, 17L, 0L, 
    6L, 7L, 1L, 
    6L, 17L, 0L, 
    6L, 18L, 0L, 
    6L, 19L, 0L, # 
    7L, 8L, 1L, 
    # 7L, 18L, 0L, 
    7L, 19L, 0L, 
    # 7L, 20L, 0L, 
    8L, 9L, 1L, 
    8L, 19L, 0L, # 
    8L, 20L, 0L, 
    9L, 10L, 1L, 
    9L, 20L, 0L, 
    9L, 21L, 0L, 
    9L, 22L, 0L, 
    10L, 11L, 1L, 
    10L, 22L, 0L, 
    10L, 23L, 0L, 
    11L, 12L, 1L, 
    11L, 23L, 0L, 
    12L, 13L, 0L, 
    12L, 23L, 0L, 
    12L, 24L, 0L, 
    13L, 14L, 1L, 
    13L, 24L, 1L, 
    14L, 15L, 1L, 
    15L, 16L, 1L, 
    16L, 17L, 1L, 
    17L, 18L, 1L, 
    18L, 19L, 1L, 
    19L, 20L, 1L, 
    20L, 21L, 1L, 
    21L, 22L, 1L, 
    22L, 23L, 1L, 
    23L, 24L, 1L
  ),
  nrow = 48L, ncol = 3L, byrow = TRUE
)

par(mar = c(0, 0, 0, 0))
plot(NULL, type = "l", xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
for(i in 1:nrow(edges)){
  edge <- edges[i, ]
  A <- points[edge[1], ]
  B <- points[edge[2], ]
  lines(rbind(A, B), col = "red", lwd = 3)
}
for(i in 1:nrow(tedges)){
  tedge <- tedges[i, ]
  if(tedge[3] == 0){
    A <- points[tedge[1], ]
    B <- points[tedge[2], ]
    lines(rbind(A, B), col = "green")
  }
}


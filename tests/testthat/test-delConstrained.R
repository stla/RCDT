test_that("2d constrained Delaunay", {
  # skip_on_os("mac")
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
  # constrained Delaunay triangulation
  del <- delaunay(points, edges) 
  #
  edges <- matrix(
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
  colnames(edges) <- c("v1", "v2", "border")
  constraints <- matrix(
    c(15L, 16L, 16L, 17L, 4L, 5L, 13L, 14L, 9L, 10L, 7L, 8L, 19L, 
      20L, 20L, 21L, 13L, 24L, 1L, 12L, 22L, 23L, 17L, 18L, 6L, 7L, 
      2L, 3L, 14L, 15L, 21L, 22L, 10L, 11L, 18L, 19L, 5L, 6L, 8L, 9L, 
      11L, 12L, 3L, 4L, 23L, 24L, 1L, 2L),
    nrow = 24L, ncol = 2L, byrow = TRUE
  )
  #
  expect_equal(nrow(del[["edges"]]), 48L)
  expect_equal(sum(del[["edges"]][, 3L]), 24L)
  expect_setequal(
    unname(split(del[["constraints"]], 1L:24L)), 
    unname(split(constraints, 1L:24L))
  )
})


test_that("2d constrained Delaunay with an intersection", {
  points <- rbind(
    c(0,0), c(1,0), c(1,1), c(0,1)
  )
  # constraint edges
  edges <- rbind(c(1L, 3L), c(2L, 4L))
  # constrained Delaunay triangulation
  del <- delaunay(points, edges) 
  #
  vertices <- rbind(points, c(0.5, 0.5))
  constraints <- matrix(
    c(1L, 5L, 3L, 5L, 2L, 5L, 4L, 5L),
    nrow = 4L, ncol = 2L, byrow = TRUE
  )
  expect_equal(del[["vertices"]], vertices)
  expect_setequal(
    unname(split(del[["constraints"]], 1L:4L)), 
    unname(split(constraints, 1L:4L))
  )
})

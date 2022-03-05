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
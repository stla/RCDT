
opar <- par(mfrow = c(1, 3), mar = c(1, 1, 1, 1))
par(opar)

layout(t(1:3), widths = c(1, 1.1, 0.9), heights = c(1, 1.1, 0.9))

C <- read.csv("C:/SL/SVG/C.csv")
CC <- C / 1000
edges <- cbind(1:(nrow(CC)), c(2:nrow(CC),1))
del <- delaunay(as.matrix(CC), edges)
plotDelaunay(del, color="yellow", asp = 1, axes = FALSE, xlab = NA, ylab = NA)

D <- read.csv("C:/SL/SVG/D2.csv")
DD <- D / 900
DD <- as.matrix(DD)#[sample.int(nrow(DD)), ]
rownames(DD) <- NULL
edges <- rbind(cbind(1:64, c(2:64, 1)), cbind(65:100, c(66:100, 65)))
del <- delaunay(DD[,], edges)
plotDelaunay(del, color="yellow", asp = 1, axes = FALSE, xlab = NA, ylab = NA)

T <- read.csv("C:/SL/SVG/T.csv")
TT <- T / 1100
edges <- cbind(1:(nrow(TT)), c(2:nrow(TT),1))
del <- delaunay(as.matrix(TT), edges)
plotDelaunay(del, color="yellow", asp = 1, axes = FALSE, xlab = NA, ylab = NA)

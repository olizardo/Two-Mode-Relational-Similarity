corr.dist <- function(x) {
     r <- nrow(x)
     c <- ncol(x)
     r.c <- matrix(0, r, r)
     c.c <- matrix(0, c, c)
     r.m <- rowMeans(x)
     c.m <- colMeans(x)
     
     for (i in 1: r) {
          for (j in 1:r) {
               r.x <- x[i, ] - r.m[i]
               r.y <- x[j, ] - r.m[j]
               r.xy <- r.x * t(r.y)
               r.xx <- r.x * t(r.x)
               r.yy <- r.y * t(r.y)
               r.num <- sum(r.xy)
               r.den <- sqrt(sum(r.xx)) * sqrt(sum(r.yy))
               r.c[i, j] <- round(r.num / r.den, 2)
          }
     }
     for (i in 1: c) {
          for (j in 1:c) {
               c.x <- x[, i] - c.m[i]
               c.y <- x[, j] - c.m[j]
               c.xy <- c.x * t(c.y)
               c.xx <- c.x * t(c.x)
               c.yy <- c.y * t(c.y)
               c.num <- sum(c.xy)
               c.den <- sqrt(sum(c.xx)) * sqrt(sum(c.yy))
               c.c[i, j] <- round(c.num / c.den, 2)
          }
     }
     
     rownames(r.c) <- rownames(x)
     colnames(r.c) <- rownames(x)
     rownames(c.c) <- colnames(x)
     colnames(c.c) <- colnames(x)
     return(list(row.sims = r.c, col.sims = c.c))
}
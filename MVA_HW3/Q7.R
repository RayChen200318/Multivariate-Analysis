rm(list = ls())
y <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/T5_5_FBEETLES.DAT'), header = FALSE)
library(MASS)
result <- lda(V2 ~ V3 + V4 + V5 + V6, y)
result
c(as.matrix(y[,3:6]) %*% result$scaling)

# the discriminant coefficient vector based on the individually standardized observations
y1 <- y[y$V2 == 1, 3:6]
y2 <- y[y$V2 == 2, 3:6]
s1 <- cov(y1)
s2 <- cov(y2)
n1 <- nrow(y1)
n2 <- nrow(y2)
W <- (n1 - 1) * s1 + (n2 - 1) * s2
Spl <- W / (nrow(y) - 2)
y1s <- t(apply(y1, 1, function(t){t / sqrt(diag(Spl))}))
y2s <- t(apply(y2, 1, function(t){t / sqrt(diag(Spl))}))
ys <- data.frame(F1 = c(y1s[,1], y2s[,1]), F2 = c(y1s[,2], y2s[,2]),
                 F3 = c(y1s[,3], y2s[,3]), F4 = c(y1s[,4], y2s[,4]),
                 G = factor(c(rep(1, n1), rep(2, n2))))
result2 <- lda(G ~ F1 + F2 + F3 + F4, ys)
result2$scaling
c(as.matrix(ys[,1:4]) %*% result2$scaling)

t.test(y1$V3, y2$V3, var.equal = TRUE)
t.test(y1$V4, y2$V4, var.equal = TRUE)
t.test(y1$V5, y2$V5, var.equal = TRUE)
t.test(y1$V6, y2$V6, var.equal = TRUE)
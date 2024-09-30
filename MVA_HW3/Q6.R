rm(list = ls())
y <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/gpa-gmat.DAT'), header = FALSE)
# Group means
y1 <- y[y$V3 == 1, 1:2]
y1_mean <- colMeans(y1)
y1_mean
y2 <- y[y$V3 ==2, 1:2]
y2_mean <- colMeans(y2)
y2_mean
y3 <- y[y$V3 == 3, 1:2]
y3_mean <- colMeans(y3)
y3_mean

# Overall mean
y_mean <- (y1_mean + y2_mean + y3_mean) /3
y_mean

# pooled sample covariance matrix
n1 <- nrow(y1)
n2 <- nrow(y2)
n3 <- nrow(y3)
Spl <- ((n1 - 1) * cov(y1) + (n2 - 1) * cov(y2) + (n3 - 1) * cov(y3)) / (nrow(y) - 3)
Spl

library(ggplot2)
ggplot(y, aes(x = V1, y = V2, color = V3)) + 
  geom_point(size = 2) + 
  geom_text(aes(label = V3), vjust = -1) + 
  labs(title = "Scatterplot of GPA vs GMAT", x = "GPA", y = "GMAT score") +
  theme_minimal()  

B <- (y1_mean - y_mean) %*% t(y1_mean - y_mean) + (y2_mean - y_mean) %*% t(y2_mean - y_mean) +
  (y3_mean - y_mean) %*% t(y3_mean - y_mean)
W <- ((n1 - 1) * cov(y1) + (n2 - 1) * cov(y2) + (n3 - 1) * cov(y3))
WinvB <- solve(W) %*% B
WinvB.eig <- eigen(WinvB)
WinvB.eig

z <- as.matrix(y[,1:2]) %*% WinvB.eig$vectors
zz <- data.frame(V1 = z[,1], V2 = z[,2], V3 = y$V3)
ggplot(zz, aes(x = V1, y = V2, color = V3)) + 
  geom_point(size = 2) + 
  geom_text(aes(label = V3), vjust = -1) + 
  labs(title = "Scatterplot After Projection", x = "First discriminant", y = "Second discriminant") +
  theme_minimal()  

y0 <- c(3.21, 497)
ob1 <- log(0.3) - c(t(y0-y1_mean) %*% solve(Spl) %*% (y0-y1_mean)) / 2
ob1
ob2 <- log(0.6) - c(t(y0-y2_mean) %*% solve(Spl) %*% (y0-y2_mean)) / 2
ob2
ob3 <- log(0.1) - c(t(y0-y3_mean) %*% solve(Spl) %*% (y0-y3_mean)) / 2
ob3


library(MASS)
predict(lda(V3 ~ V1 + V2, y , prior=c(0.3, 0.6, 0.1)), data.frame(V1 = 3.21, V2 = 497))
## $
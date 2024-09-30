Y1 <- matrix(c(3,7,2,4,4,7), nrow = 3, ncol = 2, byrow = TRUE)
Y2 <- matrix(c(6,9,5,7,4,8), nrow = 3, ncol = 2, byrow = TRUE)
S1 <- cov(Y1)
S2 <- cov(Y2)

Y1_mean <- colMeans(Y1)
Y2_mean <- colMeans(Y2)

Spl <- (2 * S1 + 2 * S2)/4
dis_coefficient <- solve(Spl) %*% (Y1_mean - Y2_mean)
dis_coefficient

y0 <- c(2,7)
Z1_mean <- t(dis_coefficient) %*% Y1_mean
Z2_mean <- t(dis_coefficient) %*% Y2_mean
z0 <- t(dis_coefficient) %*% y0
(Z1_mean + Z2_mean) / 2
z0

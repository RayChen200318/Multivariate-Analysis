A <- matrix(c(4,8,8,3,6,-9), nrow=2, ncol=3,byrow = TRUE)

B <- A %*% t(A)

B.eig <- eigen(B)
B.eig

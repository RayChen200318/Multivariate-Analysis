mu <- matrix(c(3,2,4),3,1)
sigma <- matrix(c(6,1,-2,1,13,4,-2,4,4),3,3)

a1 <- matrix(c(2,-1,3),3,1)
t(a1) %*% mu
t(a1) %*% sigma %*% a1

bA <- matrix(c(1,1,1,1,-1,2),2,3,byrow = TRUE)
bA %*% mu
bA %*% sigma %*% t(bA)

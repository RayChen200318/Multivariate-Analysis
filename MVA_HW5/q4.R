rm(list = ls())
R = matrix(c(1,0.505,0.569,0.602,0.621,0.603,0.505,1,0.422,
             0.467,0.482,0.450,0.569,0.422,1,0.926,0.877,0.878,
             0.602,0.467,0.926,1,0.874,0.894,0.621,0.482,0.877,
             0.874,1,0.937,0.603,0.450,0.878,0.894,0.937,1),
           nrow = 6,ncol = 6,byrow = TRUE)
eigenvalues <- eigen(R)$values
eigenvectors <- eigen(R)$vectors
L <- eigenvectors[, 1:3] %*% diag(sqrt(eigenvalues[1:3]))
L

Psi <- diag(diag(R - L %*% t(L)))
Psi

H <- diag(L%*%t(L))
H

H / sum(eigenvalues)

print(round(R-L%*%t(L)-Psi, 5))

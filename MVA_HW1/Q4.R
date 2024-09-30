rm(list = ls())
y <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/CALCIUM.DAT'), header = FALSE)

y <- y[,-1]

z1 <- y[,1] + y[,2] + y[,3]
z2 <- 2 * y[,1] - 3 * y[,2] + 2 * y[,3]
z3 <- - y[,1] - 2 * y[,2] - 3 * y[,3]

z <- data.frame(z1,z2,z3)

mean_vector <- colMeans(z)
mean_vector
cov_matrix <- cov(z)
cov_matrix

D <- diag(1/sqrt(diag(cov_matrix)))
corr_matrix <- D %*% cov_matrix %*% D
corr_matrix

cov_matrix_eig <- eigen(cov_matrix)
cov_matrix_eig #The spectral decomposition

cov_matrix_eig$vectors %*% diag(sqrt(cov_matrix_eig$values)) %*% t(cov_matrix_eig$vectors)

chol(cov_matrix)

det(cov_matrix)


corr_matrix_eig <- eigen(corr_matrix)
corr_matrix_eig #The spectral decomposition

corr_matrix_eig$vectors %*% diag(sqrt(corr_matrix_eig$values)) %*% t(corr_matrix_eig$vectors)

chol(corr_matrix)




det(cov_matrix)
sum(diag(cov_matrix))

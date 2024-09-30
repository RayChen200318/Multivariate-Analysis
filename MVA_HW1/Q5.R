library(GGally)
rm(list = ls())
y <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/DATA_pollution.txt'), header = TRUE)
pairs(y, main = "Pairwise Scatter Plot Matrix")
ggpairs(y)


mean_vector <- colMeans(y)
mean_vector
cov_matrix <- cov(y)
cov_matrix
cor_matrix <- cor(y)
cor_matrix

y_subset <- data.matrix(y[1:5, ])
euclidean_dist_matrix <- as.matrix(dist(y_subset, method = "euclidean"))
euclidean_dist_matrix

inv_cov_matrix <- (solve(cov_matrix))
n <- nrow(y_subset)
mahalanobis_dist_matrix <-  matrix(as.double(1:25), nrow = 5, ncol = 5)
for (i in 1:5) {
  for (j in 1:5) {
    diff_vec <- as.double(y_subset[i, ] - y_subset[j, ])
    mahalanobis_dist_matrix[i,j] <- sqrt(matrix(diff_vec, nrow = 1) %*% inv_cov_matrix %*% matrix(diff_vec, ncol = 1))
  }
}
mahalanobis_dist_matrix

det(cov_matrix) # generalized sample variance
sum(diag(cov_matrix))

cov_matrix_eig <- eigen(cov_matrix)
cov_matrix_eig #The spectral decomposition
s
chol(cov_matrix)


library(plotly)
# 创建3D散点图
plot_ly(x = ~y$Wind, y = ~y$O3, z = ~y$Solar.radiation, type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = y$O3, colorscale = c('Blues','Reds'), opacity = 0.8)) %>%
  layout(title = '3D Scatter Plot',
         scene = list(xaxis = list(title = 'x-Wind'),
                      yaxis = list(title = 'y-O3'),
                      zaxis = list(title = 'z-Solar.radiation')))

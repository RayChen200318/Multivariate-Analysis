rm(list = ls())
data <- read.table("C:/Users/Ray Chen/Desktop/pollution.dat")
S <- cov(data)
R <- cor(data)
S_eigenvalues <- round(eigen(S)$values,4)
R_eigenvalues <- round(eigen(R)$values,4)
S_eigenvalues
R_eigenvalues

library(ggplot2)
library(cowplot)
S_explained_variance_proportion <- S_eigenvalues / sum(S_eigenvalues)
S_scree_data <- data.frame(
  Principal_Component = 1:length(S_explained_variance_proportion),
  Variance_Explained = S_explained_variance_proportion
)
R_explained_variance_proportion <- R_eigenvalues / sum(R_eigenvalues)
R_scree_data <- data.frame(
  Principal_Component = 1:length(R_explained_variance_proportion),
  Variance_Explained = R_explained_variance_proportion
)
scree_plot_S <- ggplot(S_scree_data, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(x = Principal_Component, y = Variance_Explained), color = "red") +
  geom_point(aes(x = Principal_Component, y = Variance_Explained), color = "red") +
  labs(title = "Scree Plot of S",
       x = "Principal Component",
       y = "Variance Explained") +
  theme_minimal()
scree_plot_R <- ggplot(R_scree_data, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(x = Principal_Component, y = Variance_Explained), color = "red") +
  geom_point(aes(x = Principal_Component, y = Variance_Explained), color = "red") +
  labs(title = "Scree Plot of R",
       x = "Principal Component",
       y = "Variance Explained") +
  theme_minimal()
combined_plot <- plot_grid(scree_plot_S, scree_plot_R, ncol = 2)
print(combined_plot)

library(FactoMineR)
library(factoextra)
pca_result_S <- PCA(data, graph = FALSE,ncp = 2, scale.unit = FALSE)


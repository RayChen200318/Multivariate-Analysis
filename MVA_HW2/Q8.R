rm(list = ls())
y <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/sweat.DAT'), header = FALSE)
y_partial <- y[,1:2]
n <- nrow(y_partial)
p <- ncol(y_partial)
cm <- colMeans(y_partial)
cM
S <- cov(y_partial)
S_inv <- solve(S)
S_inv
RHS_quantile <- p * (n-1) / (n * (n-p)) * qf(0.95,p,n-p)
RHS_quantile


S_eig <- eigen(S)
S_eig
phi <- acos(S_eig$vectors[1]) * 180 /pi
phi
semiaxes <- sqrt(S_eig$values * RHS_quantile)
semiaxes

t_quantile <- qt(0.975, n -1 )
L1 <- cM[1] - t_quantile * sqrt(S[1] / n)
R1 <- cM[1] + t_quantile * sqrt(S[1] / n)
L2 <- cM[2] - t_quantile * sqrt(S[4] / n)
R2 <- cM[2] + t_quantile * sqrt(S[4] / n)
L1
U1
L2
R2

# 载入所需的包
library(ggplot2)
library(ggforce)
df <- data.frame(y_partial)
p <- ggplot(df, aes(x = V1, y = V2)) +
  geom_point(shape = 1, color = "black", size = 3, alpha = 0.6) + 
  geom_ellipse(aes(x0 = cm[1], y0 = cm[2], a = semiaxes[1], b = semiaxes[2], angle = atan(S_eig$vectors[2,1] / S_eig$vectors[1,1])), 
               color = "deepskyblue", alpha = 0.3, linetype = "solid", size = 1) +
  geom_segment(aes(x = cm[1], y = cm[2], xend = cm[1] + semiaxes[1] * S_eig$vectors[,1][1], yend = cm[2] + semiaxes[1] * S_eig$vectors[,1][2]), 
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "blue", size = 1) + 
  geom_segment(aes(x = cm[1], y = cm[2], xend = cm[1] + semiaxes[2] * S_eig$vectors[,2][1], yend = cm[2] + semiaxes[2] * S_eig$vectors[,2][2]), 
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "blue", size = 1) +
  geom_point(aes(x = cm[1], y = cm[2]), color = "red", size = 3) +
  geom_segment(aes(x = L1, y = 0, xend = L1, yend = L2), col = "red", linetype = 2) +
  geom_segment(aes(x = L1, y = L2, xend = L1, yend = R2), col = "red") +
  geom_segment(aes(x = L1, y = R2, xend = L1, yend = 80), col = "red", linetype = 2) +
  geom_segment(aes(x = R1, y = 0, xend = R1, yend = L2), col = "red", linetype = 2) +
  geom_segment(aes(x = R1, y = L2, xend = R1, yend = R2), col = "red") +
  geom_segment(aes(x = R1, y = R2, xend = R1, yend = 80), col = "red", linetype = 2) +
  geom_segment(aes(x = 0, y = L2, xend = L1, yend = L2), col = "red", linetype = 2) +
  geom_segment(aes(x = L1, y = L2, xend = R1, yend = L2), col = "red") +
  geom_segment(aes(x = R1, y = L2, xend = 10, yend = L2), col = "red", linetype = 2) +
  geom_segment(aes(x = 0, y = R2, xend = L1, yend = R2), col = "red", linetype = 2) +
  geom_segment(aes(x = L1, y = R2, xend = R1, yend = R2), col = "red") +
  geom_segment(aes(x = R1, y = R2, xend = 10, yend = R2), col = "red", linetype = 2) +
  theme_minimal() + 
  theme_minimal() + 
  labs(x = "mu_1", y = "mu_2") + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "white", colour = "black", size = 2),
        panel.grid.major = element_line(color = "gray80"), 
        panel.grid.minor = element_blank()) 
print(p)













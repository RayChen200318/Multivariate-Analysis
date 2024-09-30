rm(list = ls())
y1 <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/male.DAT'), header = FALSE) #male data
y2 <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/female.DAT'), header = FALSE) #female data
plot(y1$V1, y1$V2, main="Scatter plot of the hook-billed kite data", xlab="Tail length", ylab="Wing length", pch=19, col="blue")

y11 <- y1[c(1:30,32:45),]
library(Hotelling)
result <- hotelling.test(y11,y2)
print(result)

library(MASS)
y_mix <- data.frame(V1 = c(y11[,1], y2[,1]), V2 = c(y11[,2], y2[,2]),
                    V3 = factor(c(rep("male", nrow(y11)), rep("female", nrow(y2)))))
fit <- lda(V3~ V1 + V2 , data = y_mix)
print(fit)

y12 <- y1
y12[31,1] <- y12[31,1] - 80
library(Hotelling)
result1 <- hotelling.test(y12,y2)
print(result1)

library(MASS)
y_mix <- data.frame(V1 = c(y12[,1], y2[,1]), V2 = c(y12[,2], y2[,2]),
                    V3 = factor(c(rep("male", nrow(y12)), rep("female", nrow(y2)))))
fit <- lda(V3~ V1 + V2 , data = y_mix)
print(fit)

n1 <- nrow(y11)
n2 <- nrow(y2)
p <- ncol(y11)
cM <- colMeans(y11) - colMeans(y2)
cM
S <- ((n1 - 1) * var(y11) + (n2 - 1) * var(y2)) / (n1 + n2 - 2)
S.inv <- solve(S)
S.inv
RHS <- (1 / n1 + 1 / n2) * p * (n1 + n2 - 2) / (n1 + n2 - 1 - p) * qf(.95, p, n1+n2-1-p)
RHS

# 定义参数
center <- c(-6.46, 1.18) # 椭圆中心
cov_matrix <- matrix(c(0.022, -0.012, -0.012, 0.012), nrow = 2) # 协方差矩阵，描述椭圆的形状和方向
threshold <- 0.282 # 椭圆的边界值

# 绘制置信域椭圆
library(car) 
ellipse_data <- ellipse(center = cM, shape = S, radius = sqrt(qchisq(0.95, df = 2) * RHS), 
                        draw = FALSE) 
plot(ellipse_data, type = 'l', asp = 1, lwd = 2, col = 'blue', 
     xlab = expression(mu[1] - mu[2]), ylab = '', xlim = range(ellipse_data[,1]),
     ylim = range(ellipse_data[,2]), main = 'Confidence Region for Difference of Means')
grid()
points(cM[1], cM[2], pch = 19, col = 'red', cex = 1.5)


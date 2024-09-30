rm(list = ls())
y <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/cost.DAT'), header = FALSE)

library(Hotelling)
y1 <- y[y$V4 == "gasoline",1:3]
y2 <- y[y$V4 == "diesel",1:3]
result <- hotelling.test(y1,y2)
print(result)

t.test(y1$V1,y2$V1, var.equal = TRUE)
t.test(y1$V2,y2$V2, var.equal = TRUE)
t.test(y1$V3,y2$V3, var.equal = TRUE)

library(MASS)
fit <- lda(V4 ~ V1 + V2 + V3, data = y)
print(fit)

coefficients <- fit$scaling
z1 <- as.matrix(y1) %*% coefficients
z2 <- as.matrix(y2) %*% coefficients
data <- c(z1,z2)
group <- factor(c(rep("Gasoline", length(z1)), rep("Diesel", length(z2))))
boxplot(data ~ group, 
        main = "Boxplot of Two Different Groups",
        xlab = "Group",
        ylab = "Values",
        col = c("lightblue", "lightgreen"),
        border = "darkblue",
        notch = TRUE)

y11 <- y1[1:23,]
result1 <- hotelling.test(y11,y2)
result1

t.test(y11$V1,y2$V1,paired = TRUE)
t.test(y11$V2,y2$V2,paired = TRUE)
t.test(y11$V3,y2$V3,paired = TRUE)

fit1 <- lda(V4 ~ V1 + V2 + V3, data = y[c(1:23,37:59),])
print(fit1)


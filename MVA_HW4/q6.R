rm(list = ls())
y <- read.table('C:/Users/Ray Chen/Desktop/MVA/amitriptyline.DAT')
colnames(y) <-  c("Y1","Y2","X1","X2","X3","X4","X5")
model1 <-lm(Y1 ~ X1+X2+X3+X4+X5,data=y)
summary(model1)
model1.subset <- step(model1, direction = "both")
summary(model1.subset)
par(mfrow=c(2,2))
plot(model1)
shapiro.test(model1$residuals)
predict.lm(model1,data.frame("X1" = 1, "X2" = 1200, "X3" = 140, "X4" = 70, "X5" = 85),interval = "prediction")

model2 <-lm(Y2 ~ X1+X2+X3+X4+X5,data=y)
summary(model2)
model2.subset <- step(model2, direction = "both")
summary(model2.subset)
par(mfrow=c(2,2))
plot(model2)
shapiro.test(model2$residuals)
predict.lm(model2,data.frame("X1" = 1, "X2" = 1200, "X3" = 140, "X4" = 70, "X5" = 85),interval = "prediction")

model3 <-lm(cbind(Y1,Y2) ~ X1+X2+X3+X4+X5,data=y)
model3$coefficients
library(mvnormtest)
mshapiro.test(t(model3$residuals))

n <- dim(y)[1]
p <- 2
q <- 5
x0 <- c(1, 1, 1200,140, 70, 85)
library(car)
E <- summary(Manova(model3))$SSPE
critical <- qf(0.95,p,n-q-p) * (p) * (n-q-1) /(n-p-q)

X <- cbind(1,as.matrix(y[,3:7]))
XX <- solve(t(X)%*%X)
fa <- ((t(as.matrix(x0))%*%XX%*%as.matrix(x0) +1) * critical)[1]
cm <- t(t(as.matrix(model3$coefficients))%*%matrix(x0))
ellipse(c(cm), shape=E*fa/(n-q-1), radius=1,col="red",lty=2,add=FALSE,ylim=c(-500,1900),
        xlab=expression(paste(y[1])),ylab=expression(paste(y[2])))
rect(41.34785, -139.8674, 1417.702, 1291.318, density = 0, col="blue",lty = 2, lwd = par("lwd"))
legend("topleft", inset=0.03,c("Multivariate regression","Two univariate regression"),
       fill=c("red","blue"))




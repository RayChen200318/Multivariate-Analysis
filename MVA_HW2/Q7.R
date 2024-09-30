y <- read.table(paste('C:/Users/Ray Chen/Desktop/MVA/company.txt'), header = TRUE)
par(mfrow = c(1,3))
qqnorm(y$x1)
qqline(y$x1)
qqnorm(y$x2)
qqline(y$x2)
qqnorm(y$x3)
qqline(y$x3)

shapiro.test(y$x1)
shapiro.test(y$x2)
shapiro.test(y$x3)

library(GGally)
ggpairs(y)

cM <-colMeans(y)
S <- cov(y)
d <- apply(y,1,function(y) t(y-cM) %*% solve(S) %*%  (y-cM ))
plot(qc <- qchisq((1:nrow(y) - 1/2) / nrow(y), df = 3), sd <- sort(d),
     xlab = expression(paste("chi_square(3) " ,"Quantile")), ylab = "Ordered distances")
oups <- which(rank(abs(qc - sd), ties.method = "random") > nrow(y) - 3)
text(qc[oups], sd[oups] - 0.25, oups)
abline(0, 1)
title(expression(paste(chi^2,(3), " Q-Q Plot")))
           
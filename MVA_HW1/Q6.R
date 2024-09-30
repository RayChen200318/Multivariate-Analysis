set.seed(42)
n <- 100

#Plot
par(mfrow = c(2,3))
X <- rnorm(n, mean = 50, sd = 10)
Y <- 0.5*X + rnorm(n, mean = 0, sd = 5) # Positive correlation
plot(X, Y, main = "Positively Correlated X and Y", xlab = "X", ylab = "Y", pch = 19)
cor(X,Y)

Y <- -0.5*X + rnorm(n, mean = 0, sd = 5) # Negative correlation
plot(X, Y, main = "Negatively Correlated X and Y", xlab = "X", ylab = "Y", pch = 19)

Y <- 2*X # Perfect positive correlation
plot(X, Y, main = "Perfectly Positive-Correlated X and Y", xlab = "X", ylab = "Y", pch = 19, ylim = range(Y))

Y <- rnorm(n, mean = 50, sd = 10) # Uncorrelated
plot(X, Y, main = "Uncorrelated X and Y", xlab = "X", ylab = "Y", pch = 19)

Y <- X^2 + rnorm(n, mean = 0, sd = 100) # Nonlinear correlation
plot(X, Y, main = "Nonlinearly Correlated X and Y", xlab = "X", ylab = "Y", pch = 19)

# Ensure positive values for X
X <- runif(n, min = 1, max = 100) # Uniform distribution for positive values

# Logarithmic relationship
Y <- 20*log(X) + rnorm(n, mean = 0, sd = 10) # Adding some noise

# Plot
plot(X, Y, main = "Nonlinearly (Logarithmic) Correlated X and Y", xlab = "X", ylab = "Y", pch = 19)

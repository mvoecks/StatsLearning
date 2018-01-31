#First define our B0 and B1.
b0 <- -2
b1 <- 1
sigma <- .5
n = 50

# Assumption A1
x1 <- sort(runif(n, min=0, max=10))
e1 <- rnorm(n, 0, sigma)
y1 <- b0 + b1*x1 + e1

# Assumption A2
# Change random error to be mean centered exponential lambda=1/sigma^2
x2 <- sort(runif(n, min=0, max=10))
e2 <- rexp(n, 1/(sigma^2))
e2 = e2 - mean(e2)
y2 <- b0 + b1*x2 + e2

# Assumption A3
# Change the random error to be mean centered exponential lambda with error dependant on the x value
x3 <- sort(runif(n, min=0, max=10))
e3 <- rexp(n, 1/(sigma^2))
e3 = e3 + (x3/10)
e3 = e3 - mean(e3)
y3 <- b0 + b1*x3 + e3

#Assumption A4
# Change the random error to be mean centered exponential lambda=1
x4 <- sort(runif(n, min=0, max=10))
e4 <- rexp(n, 1)
e4 = e4 + (x4/10)
e4 = e4 - mean(e4)
y4 <- b0 + b1*x4 + e4

attach(mtcars)
par(mfrow=c(2,2))
plot(y1~x1, main="A1: Normal Error")
abline(b0, b1)
plot(y2~x2, main="A2: Mean 0 iid Error with Same Variance")
abline(b0, b1)
plot(y3~x3, main="A3: Uncorrelated Error with Same Variance")
abline(b0, b1)
plot(y4~x4, main="A4: Uncorrelated Error")
abline(b0, b1)




rm(list = ls())

set.seed(9231066)

n <- 1e4

x1 <- rbinom(n, 1000, 0.3)
x2 <- rbinom(n, 1000, 0.5)
x3 <- rbinom(n, 2000, 0.5)

x12 <- x1 + x2
x23 <- x2 + x3

print(sprintf('The sample mean of X1 + X2 for %d samples is %.5f then p is %.1f', n, mean(x12), mean(x12) / (1000 + 1000)))
print(sprintf('The sample mean of X2 + X3 for %d samples is %.5f then p is %.1f', n, mean(x23), mean(x23) / (1000 + 2000)))

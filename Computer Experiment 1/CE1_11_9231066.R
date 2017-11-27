rm(list = ls())

set.seed(9231066)

n <- 1000
samples <- rbinom(n, 100, 0.06)
m <- mean(samples)
print(sprintf("Sample mean is %0.5f", m))

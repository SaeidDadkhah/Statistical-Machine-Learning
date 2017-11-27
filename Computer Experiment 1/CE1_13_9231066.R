rm(list = ls())

set.seed(9231066)

n <- 1000
m1 <- 0.5
sd1 <- 1

m2 <- 0
sd2 <- 2

samples1 <- rnorm(n, mean = m1, sd = sd1)
samples2 <- rnorm(n, mean = m2, sd = sd2)

res <- sqrt(samples1 ^ 2 + samples2 ^ 2)

print(sprintf('Average distance for (X, Y) where X~N(%.1f, %.1f) and Y~N(%.1f, %.1f) is %.5f',
              m1,
              sd1,
              m2,
              sd2,
              mean(res)))

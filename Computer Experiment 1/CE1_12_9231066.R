rm(list = ls())

set.seed(9231066)

n <- 1000
x <- rnorm(n, 0.5, 1)
y <- rnorm(n, 0, 2)

distance <- sum(sqrt(x^2 + y^2)) / n

print(sprintf('Average distance for (X, Y) where X~N(0.5, 1) and Y~N(0, 4) is %.5f', distance))

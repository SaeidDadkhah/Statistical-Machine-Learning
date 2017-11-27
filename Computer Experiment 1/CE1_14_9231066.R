if (!'MASS' %in% rownames(installed.packages())) {install.packages('MASS')}
library('MASS')

rm(list = ls())

set.seed(9231066)

n <- 1000
samples <- mvrnorm(n = n,
                   mu = c(0, 0),
                   Sigma = matrix(c(1, 1/2,
                                    1/2, 1/3),
                                  ncol = 2,
                                  byrow = TRUE))
x <- samples[1:1000, 1]
y <- samples[1:1000, 2]

mean.x <- sum(x) / n
mean.y <- sum(y) / n
cov.xy <- cov(x, y)

print(sprintf('The sample mean of X is %.5f', mean.x))
print(sprintf('The sample mean of Y is %.5f', mean.y))
print(sprintf('The covariance of X and Y is %.5f', cov.xy))

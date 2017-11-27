rm(list = ls())

set.seed(9231066)

m <- 10000
n <- 100
p <- 0.3
epsilon <- 0.2

for (p in c(0.3, 0.5)) {
    error <- c()
    # for (i in seq(100, 1000, by = 100)) {
    for (i in seq(10, 100, by = 10)) {
        sample.mean <- c()
        for (j in 1:m) {
            samples <- rbinom(i, 1, p)
            sample.mean <- c(sample.mean, mean(samples))
        }
        res <- abs(sample.mean - p) > epsilon
        mean.res <- mean(res)
        error <- c(error, mean.res)
        chebyshev <- p * (1 - p) / (i * epsilon ^ 2)
        rep1 <- sprintf('For p = %.1f and n = %d, P = %.10f', p, i, mean.res)
        rep2 <- 'q'
        if (mean.res < chebyshev) {
            rep2 <- sprintf('< %.5f = Chebyshev boundary.', chebyshev)
        } else {
            rep2 <- sprintf('> %.5f = Chebyshev boundary.', chebyshev)
        }
        print(sprintf('%s %s', rep1, rep2))
    }
}

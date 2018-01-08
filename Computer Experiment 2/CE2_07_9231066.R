if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')

rm(list = ls())

set.seed(9231066)

alpha <- 2
beta <- 5
n <- 100

true.mean <- alpha / (alpha + beta)
true.variance <- alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1))
true.skewness <- 2 * (beta - alpha) * sqrt(alpha + beta + 1) / ((alpha + beta + 2) * sqrt(alpha * beta))

samples <- rbeta(n, alpha, beta)
x <- seq(-0.1, 1.1, 0.005)

empirical <- function(x) {
    mean(samples < x)
}

cdf.empirical <- c()
cdf.true <- c()
for (i in x) {
    cdf.empirical <- c(cdf.empirical, empirical(i))
    cdf.true <- c(cdf.true, pbeta(i, alpha, beta))
}

plot <- ggplot(data = data.frame(cdf.empirical, cdf.true), aes(x = x))
plot <- plot + geom_line(aes(y = cdf.empirical, color = 'Empirical CDF'))
plot <- plot + geom_line(aes(y = cdf.true, color = 'Beta Dist. CDF'))
plot <- plot + scale_color_discrete(name = "Legend")
show(plot)

estimate.mean <- sum(samples) / n
estimate.var1 <- sum((samples - estimate.mean)^2) / n
estimate.var2 <- sum((samples - estimate.mean)^2) / (n - 1)
estimate.skewness <- sum((samples - estimate.mean)^3) / (n * sqrt(estimate.var1)^3)

print(sprintf('Mean is %f and estimate of it is %f.', true.mean, estimate.mean))
print(sprintf('Variance is %f and estimates of it are %f and %f.', true.variance, estimate.var1, estimate.var2))
print(sprintf('Skewness is %f and estimate of it is %f.', true.skewness, estimate.skewness))


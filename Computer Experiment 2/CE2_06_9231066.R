if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

n <- 100
mu <- 10
sigma <- 1
draws <- 1000

B <- 1000
alpha <- 0.05

samples <- rnorm(n, mean = mu, sd = sigma)

posterior.mean <- mean(samples)
posterior.variance <- 1 / n

d <- function(x) {dnorm(x, mean = posterior.mean, sd = sqrt(posterior.variance))}
plot.pdf <- ggplot(data = data.frame(posterior.pdf = c(9.5, 10.5)), mapping = aes(x = posterior.pdf))
plot.pdf <- plot.pdf + stat_function(fun = d, n = 1000, color = '#56B4E9')

posterior.draws <- rnorm(draws, mean = posterior.mean, sd = sqrt(posterior.variance))

plot.hist <- ggplot(data = data.frame(posterior.draws), aes(x = posterior.draws))
plot.hist <- plot.hist + geom_histogram(bins = 30, fill = '#56B4E9')
plot.hist <- plot.hist + xlim(9.5, 10.5)

grid.arrange(plot.pdf, plot.hist, nrow = 2)

s.estimate <- exp(mean(posterior.draws))
s.bootstrap <- c()
s.true <- c()
for (i in 1:B) {
    tmp <- rnorm(1, mean = posterior.mean, sd = sqrt(posterior.variance))
    samples <- rnorm(draws, mean = tmp, sd = 1)
    s.bootstrap <- c(s.bootstrap, exp(mean(samples)))
}

d <- function(x) {
    sqrt(n) / (x*sigma*sqrt(2*pi)) * exp(-n*(log(x) - posterior.mean)^2 / (2*sigma^2))
}
plot.pdf <- ggplot(data = data.frame(theta.pdf = c(0, 40000)), mapping = aes(x = theta.pdf))
plot.pdf <- plot.pdf + stat_function(fun = d, n = 2500, color = '#56B4E9')

posterior.draws <- rnorm(draws, mean = posterior.mean, sd = sqrt(posterior.variance))

plot.hist <- ggplot(data = data.frame(s.bootstrap), aes(x = s.bootstrap))
plot.hist <- plot.hist + geom_histogram(bins = 50, fill = '#56B4E9')
plot.hist <- plot.hist + xlim(0, 40000)

grid.arrange(plot.pdf, plot.hist, nrow = 2)

s.bootstrap <- sort(s.bootstrap)
s.stderr <- sd(s.bootstrap)

s.normal.min <- s.estimate + s.stderr * qnorm(alpha / 2)
s.normal.max <- s.estimate + s.stderr * qnorm(1 - alpha / 2)
s.pivotal.min <- 2 * s.estimate - s.bootstrap[B * (1 - alpha / 2)]
s.pivotal.max <- 2 * s.estimate - s.bootstrap[B * alpha / 2]
s.percentile.min <- s.bootstrap[B * alpha / 2]
s.percentile.max <- s.bootstrap[B * (1 - alpha / 2)]

print(sprintf('Normal confidence interval is (%f, %f).', s.normal.min, s.normal.max))
print(sprintf('Pivotal confidence interval is (%f, %f).', s.pivotal.min, s.pivotal.max))
print(sprintf('Percentile confidence interval is (%f, %f).', s.percentile.min, s.percentile.max))

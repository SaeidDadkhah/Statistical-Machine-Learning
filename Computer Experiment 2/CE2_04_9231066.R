if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')

rm(list = ls())

set.seed(9231066)

data <- read.csv('data/Earthquakes.txt')
mag <- data[['mag']]

n <- length(mag)
B <- 1000
alpha <- 0.05
epsilon <- sqrt(log(2/alpha) / (2*n))

empirical <- function(x) {
    mean(mag < x)
}

x <- seq(3, 7, 0.01)
cdf.empirical <- c()
cdf.min <- c()
cdf.max <- c()
for (i in x) {
    emp <- empirical(i)
    cdf.empirical <- c(cdf.empirical, emp)
    cdf.min <- c(cdf.min, max(emp - epsilon, 0))
    cdf.max <- c(cdf.max, min(emp + epsilon, 1))
}

plot <- ggplot(data = data.frame(cdf.empirical, cdf.min, cdf.max), aes(x = x))
plot <- plot + geom_line(aes(y = cdf.empirical, color = 'Empirical CDF'))
plot <- plot + geom_ribbon(aes(ymin = cdf.min, ymax = cdf.max), alpha = 0.3)
plot <- plot + scale_color_discrete(name = "Legend")
show(plot)

s.estimate <- empirical(4.9) - empirical(4.3)
s.bootstrap <- c()

samples <- c()
empirical <- function(x) {
    mean(samples < x)
}
for (b in 1:B) {
    samples <- sample(mag, n, replace = TRUE)
    s.bootstrap <- c(s.bootstrap, empirical(4.9) - empirical(4.3))
}

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

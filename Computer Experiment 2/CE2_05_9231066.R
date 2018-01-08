if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')

rm(list = ls())

set.seed(9231066)

B <- 1000
n <- 100
alpha <- 0.05

data <- rnorm(n = n, mean = 10, sd = 10)
mean <- mean(data)

s.estimate <- exp(mean(data))
s.bootstrap <- c()
s.true <- c()
for (i in 1:B) {
    samples <- sample(data, n, replace = TRUE)
    
    s.bootstrap <- c(s.bootstrap, exp(mean(samples)))
    
    tmp.mean <- rnorm(n = 1, mean = mean, sd = 10 / 100)
    tmp.data <- rnorm(n = n, mean = tmp.mean, sd = 10)
    s.true <- c(s.true, exp(mean(tmp.data)))
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

dat <- data.frame(x = c(s.bootstrap, s.true), y = rep(c('Bootstrap', 'True'), each = B))

plot <- ggplot(data = dat, aes(x = x, fill = y))
plot <- plot + geom_histogram(data = subset(dat, y == 'Bootstrap'), aes(fill = y), binwidth = 7500, alpha = 0.7)
plot <- plot + geom_histogram(data = subset(dat, y == 'True'), aes(fill = y), binwidth = 7500, alpha = 0.7)
plot <- plot + scale_fill_manual(name = 'Legend', values = c('red', 'darkgrey'))
plot <- plot + xlim(0, 400000)
show(plot)

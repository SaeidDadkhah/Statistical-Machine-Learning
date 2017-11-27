if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

plot.unif <- ggplot(data = data.frame(x = c(-1, 1)), mapping = aes(x = x))
plot.unif <- plot.unif + stat_function(fun = dunif, n = 1000, color = '#56B4E9')
plot.unif <- plot.unif + xlim(-0.1, 1.1)
plot.unif

n <- 100
x <- 1:n
real.mean <- rep(1/2, n)
real.variance <- 1 / (12 * x)

plot.real <- ggplot(data = data.frame(real.mean, real.variance), aes(x = x))
plot.real <- plot.real + geom_line(aes(y = real.mean, color = '#56B4E9'))
plot.real <- plot.real + geom_line(aes(y = real.variance, color = '#FF9999'))
plot.real <- plot.real + scale_color_discrete(name = "Legend", labels = c('Mean', 'Variance'))
plot.real

n <- c(1, 10, 30, 100)
mean.mean <- c()
mean.variance <- c()
for (i in n) {
    sample.mean <- c()
    for (j in 1:100) {
        samples <- runif(i)
        current.mean <- mean(samples)
        sample.mean <- c(sample.mean, current.mean)
    }
    mean.mean <- c(mean.mean, mean(sample.mean))
    mean.variance <- c(mean.variance, var(sample.mean))
}

plot.samples <- ggplot(data = data.frame(mean.mean, mean.variance), aes(x = n))
plot.samples <- plot.samples + geom_line(aes(y = mean.mean, color = '#56B4E9'))
plot.samples <- plot.samples + geom_line(aes(y = mean.variance, color = '#FF9999'))
plot.samples <- plot.samples + scale_color_discrete(name = "Legend", labels = c('Mean', 'Variance'))
plot.samples

grid.arrange(plot.unif, plot.samples, nrow = 2)

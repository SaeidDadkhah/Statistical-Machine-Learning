if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

pdf <- function(y) {
    exp(-log(y)^2 / 2) / (y * sqrt(2*pi))
}

xmax <- 15
n <- 10000
y.samples <- exp(rnorm(n))

plot.pdf <- ggplot(data = data.frame(x = c(-1, 50)), mapping = aes(x = x))
plot.pdf <- plot.pdf + stat_function(fun = pdf, n = 1000, geom = 'area', fill = '#56B4E9')
plot.pdf <- plot.pdf + xlim(-.1, xmax)
plot.pdf

plot.sim <- ggplot(data = data.frame(y.samples), aes(x = y.samples))
plot.sim <- plot.sim + geom_histogram(bins = 200, fill = '#56B4E9')
plot.sim <- plot.sim + xlim(-.1, xmax)
plot.sim

grid.arrange(plot.pdf, plot.sim, nrow = 2)

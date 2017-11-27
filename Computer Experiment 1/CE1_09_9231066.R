if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

n <- 10000
x <- 1:n
samples.norm <- rnorm(n)
mean.norm <- cumsum(samples.norm) / x

samples.cauchy <- rcauchy(n)
mean.cauchy <- cumsum(samples.cauchy) / x

plot.norm <- ggplot(data = data.frame(mean.norm), aes(x = x, y = mean.norm))
plot.norm <- plot.norm + geom_line(color = '#56B4E9')
plot.norm

plot.cauchy <- ggplot(data = data.frame(mean.cauchy), aes(x = x, y = mean.cauchy))
plot.cauchy <- plot.cauchy + geom_line(color = '#56B4E9')
plot.cauchy

grid.arrange(plot.norm, plot.cauchy, nrow = 2)

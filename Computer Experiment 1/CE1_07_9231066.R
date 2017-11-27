if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

prob <- c(rep(3 / 20, 5), 1 / 4)

n <- 10
throws <- sample(1:6, n, replace = TRUE, prob = prob)
pdf <- data.frame(table(throws) / n)

plot.sim <- ggplot(data = data.frame(pdf), aes(x = throws, weight = Freq))
plot.sim <- plot.sim + geom_bar(fill = '#56B4E9')

plot.true <- ggplot(data = data.frame(prob), aes(x = 1:6, weight = prob))
plot.true <- plot.true + geom_bar(fill = '#56B4E9')

grid.arrange(plot.sim, plot.true, nrow = 2)

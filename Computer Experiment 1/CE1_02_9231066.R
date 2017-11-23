if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

flip <- function(n, p) {
    x <- seq(1, n)
    coin <- rbinom(n, 1, p)
    s <- cumsum(coin)
    data.frame(x, s)
}

plot.flip <- function(df, n, p) {
    plot <- ggplot(data = df, aes(x = x, y = s))
    plot <- plot + geom_line(color = 'blue')
    plot <- plot + geom_hline(yintercept = n * p, color = 'red')
    plot
}

df.10 <- flip(10, 0.4)
plot.10 <- plot.flip(df.10, 10, 0.4)

df.100 <- flip(100, 0.4)
plot.100 <- plot.flip(df.100, 100, 0.4)

df.1000 <- flip(1000, 0.4)
plot.1000 <- plot.flip(df.1000, 1000, 0.4)

grid.arrange(plot.10, plot.100, plot.1000, nrow = 3)

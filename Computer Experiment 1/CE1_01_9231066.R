if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

flip <- function(p) {
    coin <- rbinom(1000, 1, p)
    
    x <- seq(1, 1000, by = 1)
    s <- cumsum(coin)
    y <- s / x
    data.frame(x, y)
}

plot.flip <- function(df, p) {
    plot <- ggplot(data = df, aes(x = x, y = y))
    plot <- plot + geom_line(color = 'blue')
    plot <- plot + geom_hline(yintercept = p, color = 'red')
    plot
}

coin.4 <- flip(0.4)
plot.4 <- plot.flip(coin.4, 0.4)

coin.04 <- flip(0.04)
plot.04 <- plot.flip(coin.04, 0.04)

grid.arrange(plot.4, plot.04, nrow = 2)

if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

m <- 1000
n <- 100
alpha <- 0.1
x <- seq(-3, 3, 0.01)

inside <- c()

# r <- rnorm
# p <- pnorm
r <- rcauchy
p <- pcauchy

for (i in 1:m) {
    samples <- r(n)
    
    pdf.empirical <- function(x) {
        mean(samples < x)
    }
    
    epsilon <- sqrt(log(2/alpha) / (2*n))
    
    cdf.empirical <- c()
    cdf.min <- c()
    cdf.max <- c()
    cdf.true <- c()
    
    cur.inside <- TRUE
    for (i in x) {
        cur.emp <- pdf.empirical(i)
        cur.min <- max(cur.emp - epsilon, 0)
        cur.max <- min(cur.emp + epsilon, 1)
        cur.true <- p(i)
        
        cdf.empirical <- c(cdf.empirical, cur.emp)
        cdf.min <- c(cdf.min, cur.min)
        cdf.max <- c(cdf.max, cur.max)
        cdf.true <- c(cdf.true, cur.true)
        
        if (cur.true < cur.min || cur.max < cur.true) {
            cur.inside <- FALSE
        }
    }
    
    if (cur.inside) {
        dfs.in <- data.frame(cdf.empirical, cdf.min, cdf.max, cdf.true)
    } else {
        dfs.out <- data.frame(cdf.empirical, cdf.min, cdf.max, cdf.true)
    }
    
    inside <- c(inside, cur.inside)
}

print(sprintf('%.3f', mean(inside)))

plot.in <- ggplot(data = dfs.in, aes(x = x))
plot.in <- plot.in + geom_line(aes(y = cdf.empirical, color = 'Empirical CDF'))
plot.in <- plot.in + geom_ribbon(aes(ymin = cdf.min, ymax = cdf.max), alpha = 0.3)
plot.in <- plot.in + geom_line(aes(y = cdf.true, color = 'Normal Dist. CDF'))
plot.in <- plot.in + scale_color_discrete(name = "Legend")

plot.out <- ggplot(data = dfs.out, aes(x = x))
plot.out <- plot.out + geom_line(aes(y = cdf.empirical, color = 'Empirical CDF'))
plot.out <- plot.out + geom_ribbon(aes(ymin = cdf.min, ymax = cdf.max), alpha = 0.3)
plot.out <- plot.out + geom_line(aes(y = cdf.true, color = 'Normal Dist. CDF'))
plot.out <- plot.out + scale_color_discrete(name = "Legend")

grid.arrange(plot.in, plot.out, nrow = 2)

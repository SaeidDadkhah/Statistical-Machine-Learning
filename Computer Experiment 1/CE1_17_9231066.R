if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')

rm(list = ls())

set.seed(9231066)

# Plot CDF
plot.unif <- ggplot(data = data.frame(x = c(-5, 5)), mapping = aes(x = x))
plot.unif <- plot.unif + stat_function(fun = function(x) pnorm(x, mean = 0, sd = 1), n = 1000, color = '#56B4E9')
plot.unif <- plot.unif + stat_function(fun = function(x) pnorm(x, mean = 0, sd = .1), n = 1000, color = 10)
plot.unif <- plot.unif + stat_function(fun = function(x) pnorm(x, mean = 0, sd = .01), n = 1000, color = 100)
plot.unif <- plot.unif + stat_function(fun = function(x) pnorm(x, mean = 0, sd = .001), n = 1000, color = 1000)
plot.unif <- plot.unif + stat_function(fun = function(x) pnorm(x, mean = 0, sd = .0001), n = 1000, color = 10000)
plot.unif <- plot.unif + xlim(-2, 2)
show(plot.unif)

# Convergence in Distribution
## Discrete Uniform CDF
pdu <- function(x) ifelse(x < 0, 0, 1)

plot.dist <- function(epsilon) {
    n <- 1000
    
    i.last <- 1
    p.last <- 1
    while (p.last != 0) {
        samples <- rnorm(n, mean = 0, sd = i.last)
        p.last <- mean(abs(samples) > epsilon)
        i.last <- i.last / 2
    }
    
    plot.p <- ggplot(data = data.frame(x = c(-1e-5, 1e-5)), mapping = aes(x = x))
    plot.p <- plot.p + stat_function(fun = function(x) pnorm(x, sd = i.last), n = 1000, color = '#56B4E9')
    plot.p <- plot.p + stat_function(fun = pdu, n = 1000, color = '#FF9999')
    plot.p
}

plot.1 <- plot.dist(1e-1)
plot.2 <- plot.dist(1e-2)
plot.3 <- plot.dist(1e-3)
plot.4 <- plot.dist(1e-4)
plot.5 <- plot.dist(1e-5)
plot.6 <- plot.dist(1e-6)

grid.arrange(plot.1, plot.2, plot.3, plot.4, plot.5, plot.6, nrow = 3)

# Convergence in Probability
plot.prob <- function(epsilon) {
    n <- 1000
    
    i.last <- 1
    i.list <- c()
    p.last <- 1
    p.list <- c()
    samples.mean <- c()
    while (p.last != 0) {
        samples.list <- rnorm(n, mean = 0, sd = i.last)
        samples.mean <- c(samples.mean, mean(mean(abs(samples.list))))
        p.last <- mean(abs(samples.list) > epsilon)
        p.list <- c(p.list, p.last)
        
        i.list <- c(i.list, i.last)
        i.last <- i.last / 2
    }
    
    plot.p <- ggplot(data = data.frame(i.list, p.list, samples.mean), mapping = aes(x = 1:length(i.list)))
    plot.p <- plot.p + geom_line(aes(y = p.list, color = '#56B4E9'))
    plot.p <- plot.p + geom_line(aes(y = samples.mean, color = '#FF9999'))
    plot.p <- plot.p + scale_color_discrete(name = "Legend", labels = c('P(|X| > e)', 'Sample Mean'))
    plot.p
}

plot.1 <- plot.prob(1e-1)
plot.3 <- plot.prob(1e-3)
plot.8 <- plot.prob(1e-8)
plot.10 <- plot.prob(1e-10)
plot.15 <- plot.prob(1e-15)
plot.20 <- plot.prob(1e-20)

grid.arrange(plot.1, plot.3, plot.8, plot.10, plot.15, plot.20, nrow = 3)

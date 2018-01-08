if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')

rm(list = ls())

set.seed(9231066)

p <- function(x) {
    (dnorm(x, mean = 3, sd = sqrt(0.5)) + dnorm(x, mean = 7, sd = sqrt(0.5))) / 2
}

q1 <- function(x) {dnorm(x, mean = 3, sd = sqrt(0.5))}
q2 <- function(x) {dnorm(x, mean = 5, sd = sqrt(1.5))}
q3 <- function(x) {dnorm(x, mean = 7, sd = sqrt(0.5))}

f <- NULL
g <- NULL

kullback <- function(x) {
    f(x) * log(f(x) / g(x))
}

f <- p
g <- q1
kl <- integrate(kullback, 0, 10)
print(sprintf('D(p, q1) = %f', kl[["value"]]))
f <- q1
g <- p
kl <- integrate(kullback, 0, 10)
print(sprintf('D(q1, p) = %f', kl[["value"]]))

f <- p
g <- q2
kl <- integrate(kullback, 0, 10)
print(sprintf('D(p, q2) = %f', kl[["value"]]))
f <- q2
g <- p
kl <- integrate(kullback, 0, 10)
print(sprintf('D(q2, p) = %f', kl[["value"]]))

f <- p
g <- q3
kl <- integrate(kullback, 0, 10)
print(sprintf('D(p, q3) = %f', kl[["value"]]))
f <- q3
g <- p
kl <- integrate(kullback, 0, 10)
print(sprintf('D(q3, p) = %f', kl[["value"]]))

plot.pdf <- ggplot(data = data.frame(x = c(0, 10)), mapping = aes(x = x))
plot.pdf <- plot.pdf + stat_function(aes(color = 'p'), fun = p, n = 1000)
plot.pdf <- plot.pdf + stat_function(aes(color = 'q2*'), fun = q1, n = 1000)
plot.pdf <- plot.pdf + stat_function(aes(color = 'q1*'), fun = q2, n = 1000)
plot.pdf <- plot.pdf + scale_color_discrete(name = "Legend")
show(plot.pdf)

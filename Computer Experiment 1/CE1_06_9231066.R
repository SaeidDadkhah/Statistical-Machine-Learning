if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'VennDiagram' %in% rownames(installed.packages())) {install.packages('VennDiagram')}
library('VennDiagram')

rm(list = ls())

set.seed(9231066)

A <- c(2, 3, 6)
B <- c(1, 2, 3, 4)

# A <- c(1, 2, 3)
# B <- c(4, 6)

p.a <- 0
p.b <- 0
p.ab <- 0

n <- 1000
x <- 1:n
tosses <- sample(1:6, n, replace = TRUE)

a <- tosses %in% A
b <- tosses %in% B
ab <- tosses %in% A & tosses %in% B

p.a <- cumsum(a) / x
p.b <- cumsum(b) / x
p.ab <- cumsum(ab) / x
p.AmulB <- p.a * p.b

plot <- ggplot(data = data.frame(p.a, p.b, p.ab, p.AmulB))
plot <- plot + geom_line(aes(x = 1:n, y = p.a, color = 'P(A)'))
plot <- plot + geom_line(aes(x = 1:n, y = p.b, color = 'P(B)'))
plot <- plot + geom_line(aes(x = 1:n, y = p.ab, color = 'P(AB)'))
plot <- plot + geom_line(aes(x = 1:n, y = p.AmulB, color = 'P(A) * P(B)'))
plot <- plot + geom_line(aes(x = 1:n, y = p.ab - p.AmulB, color = 'P(AB) - P(A) * P(B)'))
plot <- plot + geom_hline(yintercept = 0, color = 'black')
plot <- plot + ylab("Probability")
plot <- plot + xlab("Number of Tosses")
plot <- plot + scale_color_discrete(name = "Legend")
plot

grid.newpage()
draw.pairwise.venn(p.a[n] * n,
                   p.b[n] * n,
                   p.ab[n] * n,
                   category = c("A", "B"),
                   lty = rep("blank", 2),
                   fill = c("light blue", "pink"),
                   alpha = rep(0.5, 2),
                   cat.pos = c(0, 
                                                                                                                                                        0), cat.dist = rep(0.025, 2))

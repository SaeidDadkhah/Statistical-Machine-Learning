rm(list = ls())

set.seed(9231066)

B <- 1000

data <- read.table('data/Old Faithful Geyser Data.txt', skip = 20)

eruption.mean.estimate <- mean(data[['eruptions']])

n <- length(data[['eruptions']])
eruption.mean.bootstrap <- c()
for (i in 1:B) {
    samples <- sample(data[['eruptions']], n, replace = TRUE)
    eruption.mean.bootstrap <- c(eruption.mean.bootstrap, mean(samples))
}
eruption.mean.stderr <- sd(eruption.mean.bootstrap)
print(sprintf('Eruption mean is %f and standard error is %f.', eruption.mean.estimate, eruption.mean.stderr))

alpha <- 0.1
eruption.mean.min <- eruption.mean.estimate + eruption.mean.stderr * qnorm(alpha / 2)
eruption.mean.max <- eruption.mean.estimate + eruption.mean.stderr * qnorm(1 - alpha / 2)
print(sprintf('90%% normal confidence interval for eruption mean is (%f, %f).', eruption.mean.min, eruption.mean.max))

eruption.median.estimate <- median(data[['eruptions']])

n <- length(data[['eruptions']])
eruption.median.bootstrap <- c()
for (i in 1:B) {
    samples <- sample(data[['eruptions']], n, replace = TRUE)
    eruption.median.bootstrap <- c(eruption.median.bootstrap, median(samples))
}
eruption.median.stderr <- sd(eruption.median.bootstrap)
print(sprintf('Eruption median is %f and standard error is %f.', eruption.median.estimate, eruption.median.stderr))

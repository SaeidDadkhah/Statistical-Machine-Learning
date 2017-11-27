rm(list = ls())

set.seed(9231066)

total.probability <- 0

experiments <- 100000

birthdays <- c()
for (i in 1:experiments) {
    birthday <- sample(1:365, 28, replace = TRUE)
    birthday <- sum(birthday == 1)
    birthdays <- c(birthdays, birthday)
}

print(sprintf('Total probability is %0.12f', mean(birthdays >= 2)))

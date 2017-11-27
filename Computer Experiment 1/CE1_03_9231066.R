rm(list = ls())

set.seed(9231066)

prize <- sample(1:3, 2000, replace = TRUE)
select <- sample(1:3, 2000, replace = TRUE)
select.true <- sample(0, 1000, replace = TRUE)

doors <- 1:3

for (i in seq(1, 1000)) {
    monty.select <- doors[-c(select[i], prize[i])]
    if (length(monty.select) == 1) {
        select.true[i] = doors[-c(select[i], monty.select)]
    } else {
        monty.select <- sample(monty.select, 1)
        select.true[i] <- c(doors[-c(select[i], monty.select)])
    }
}

win.1 <- prize[1:1000] == select.true
win.2 <- prize[1001:2000] == select[1001:2000]
print(sprintf("Winning probability if we change the door: %0.5f", mean(win.1)))
print(sprintf("Winning probability if we don't change the door: %0.5f", mean(win.2)))

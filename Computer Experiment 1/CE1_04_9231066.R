rm(list = ls())

m <- 5
sd <- sqrt(18)

print(sprintf('P(X < 9) = %0.5f', pnorm(9, mean = m, sd = sd)))
print(sprintf('P(X > -3) = %0.5f', pnorm(-3, mean = m, sd = sd, lower.tail = FALSE)))
print(sprintf('P(X > x) = 0.05 ==> x = %0.5f', qnorm(0.05, mean = m, sd = sd, lower.tail = FALSE)))
print(sprintf('P(0 <= X < 4) = %0.5f', pnorm(4, mean = m, sd = sd) - pnorm(0, mean = m, sd = sd)))

x.min <- m + sd
x.mid <- 0
x.max <- m + 3 * sd

while (x.max - x.min > 1e-10) {
    x.mid <- (x.min + x.max) / 2
    q.mid <- pnorm(x.mid, mean = m, sd = sd, lower.tail = FALSE) +
        pnorm(-x.mid, mean = m, sd = sd)
    if (q.mid < 0.05) {
        x.max <- x.mid
    } else {
        x.min <- x.mid
    }
}

print(sprintf('P(|X - mu| > |x|) = 0.05 ==> x = %0.5f5', x.mid))

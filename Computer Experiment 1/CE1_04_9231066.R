rm(list = ls())

m <- 5
sd <- sqrt(18)

sprintf('P(X < 9) = %0.5f', pnorm(9, mean = m, sd = sd))
sprintf('P(X > -3) = %0.5f', pnorm(-3, mean = m, sd = sd, lower.tail = FALSE))
sprintf('P(X > x) = 0.05 ==> x = %0.5f', qnorm(0.05, mean = m, sd = sd, lower.tail = FALSE))
sprintf('P(0 <= X < 4) = %0.5f', pnorm(4, mean = m, sd = sd) - pnorm(0, mean = m, sd = sd))
sprintf('P(|X - mu| > |x|) = 0.05 ==> x = %0.5f5', qnorm(0.05 / 2, mean = m, sd = sd))

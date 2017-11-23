total.probability <- 0

for (i in seq(2, 28)) {
    total.probability <- total.probability + choose(28, i) * (1/365)^i * (364/365)^(28-i)
}

sprintf('Total probability is %0.12f', total.probability)

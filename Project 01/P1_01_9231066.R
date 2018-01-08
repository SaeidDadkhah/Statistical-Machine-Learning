if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')
if (!'reshape2' %in% rownames(installed.packages())) {install.packages('reshape2')}
library('reshape2')

# delete old data
rm(list = ls())

# read and split data
data <- read.csv('data/Dataset1.csv', header = FALSE)
train <- data[1:400,]
test <- data[401:500,]

# a) scatter plots
plot.a.1 <- ggplot(data, aes(x = V1, y = V9))
plot.a.1 <- plot.a.1 + geom_point(color = '#56B4E9')
# plot.a.1 <- plot.a.1 + geom_smooth(method = 'lm', color = 'red')

plot.a.2 <- ggplot(data, aes(x = V2, y = V9))
plot.a.2 <- plot.a.2 + geom_point(color = '#56B4E9')
# plot.a.2 <- plot.a.2 + geom_smooth(method = 'lm', color = 'red')

plot.a.3 <- ggplot(data, aes(x = V3, y = V9))
plot.a.3 <- plot.a.3 + geom_point(color = '#56B4E9')
# plot.a.3 <- plot.a.3 + geom_smooth(method = 'lm', color = 'red')

plot.a.4 <- ggplot(data, aes(x = V4, y = V9))
plot.a.4 <- plot.a.4 + geom_point(color = '#56B4E9')
# plot.a.4 <- plot.a.4 + geom_smooth(method = 'lm', color = 'red')

plot.a.5 <- ggplot(data, aes(x = V5, y = V9))
plot.a.5 <- plot.a.5 + geom_point(color = '#56B4E9')
# plot.a.5 <- plot.a.5 + geom_smooth(method = 'lm', color = 'red')

plot.a.6 <- ggplot(data, aes(x = V6, y = V9))
plot.a.6 <- plot.a.6 + geom_point(color = '#56B4E9')
# plot.a.6 <- plot.a.6 + geom_smooth(method = 'lm', color = 'red')

plot.a.7 <- ggplot(data, aes(x = V7, y = V9))
plot.a.7 <- plot.a.7 + geom_point(color = '#56B4E9')
# plot.a.7 <- plot.a.7 + geom_smooth(method = 'lm', color = 'red')

plot.a.8 <- ggplot(data, aes(x = V8, y = V9))
plot.a.8 <- plot.a.8 + geom_point(color = '#56B4E9')
# plot.a.8 <- plot.a.8 + geom_smooth(method = 'lm', color = 'red')

# grid.arrange(plot.a.1, plot.a.2, plot.a.3, plot.a.4, plot.a.5, plot.a.6, plot.a.7, plot.a.8, nrow = 2, ncol = 4)

cormat <- cor(data)
cormat[lower.tri(cormat)] <- NA
melted_cormat <- melt(cormat, na.rm = TRUE)
plot.cor <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))
plot.cor <- plot.cor + geom_tile(color = "white")
plot.cor <- plot.cor + scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limit = c(-1,1),
    space = "Lab",
    name = "Pearson\nCorrelation")
plot.cor <- plot.cor + theme_minimal()
plot.cor <- plot.cor + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
plot.cor <- plot.cor + coord_fixed()
plot.cor <- plot.cor + geom_text(aes(Var2, Var1, label = round(value, 2)), color = "black", size = 4)
plot.cor <- plot.cor + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.7),
    legend.direction = "horizontal")
plot.cor <- plot.cor + guides(fill = guide_colorbar(barwidth = 12, barheight = 2, title.position = "top", title.hjust = 0.5))
# show(plot.cor)

# b) simple least squares
print("|========================================|")
print("|                                        |")
print("|>>>>> Part B: Select First Feature <<<<<|")
print("|                                        |")
print("|________________________________________|")

models <- list()
for (i in 1:8) {
    models[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models[[i]]$d <- data.frame(X = train[[x]], Y = train[['V9']])
    
    models[[i]]$slope <- list()
    models[[i]]$intercept <- list()
    models[[i]]$slope$estimate <- cov(models[[i]]$d$X, models[[i]]$d$Y) / var(models[[i]]$d$X)
    models[[i]]$intercept$estimate <- mean(models[[i]]$d$Y) - models[[i]]$slope$estimate * mean(models[[i]]$d$X)
    models[[i]]$predictor <- function(x) {models[[i]]$intercept$estimate + models[[i]]$slope$estimate * x}
    
    models[[i]]$plot <- ggplot(data = models[[i]]$d, aes(x = X, y = Y))
    models[[i]]$plot <- models[[i]]$plot + geom_point(color = '#56B4E9')
    
    models[[i]]$train <- list()
    models[[i]]$train$prediction <- models[[i]]$predictor(models[[i]]$d$X)
    models[[i]]$train$rss <- sum((models[[i]]$d$Y - models[[i]]$train$prediction) ^ 2)
    models[[i]]$train$tss <- var(models[[i]]$d$Y) * nrow(models[[i]]$d)
    models[[i]]$train$r2 <- 1 - models[[i]]$train$rss / models[[i]]$train$tss
    models[[i]]$train$sigma2 <- models[[i]]$train$rss / nrow(models[[i]]$d)
    models[[i]]$train$AIC <- nrow(models[[i]]$d) *
        (log(2*pi) + 1 + log(models[[i]]$train$rss / nrow(models[[i]]$d))) +
        ((2 + 1) * 2)
    
    m <- mean(models[[i]]$d$X)
    models[[i]]$slope$sd <- sqrt(models[[i]]$train$sigma2 / sum((models[[i]]$d$X - m) ^ 2))
    models[[i]]$intercept$sd <- sqrt(models[[i]]$train$sigma2 / sum((models[[i]]$d$X - m) ^ 2)) * 
        sqrt(sum(models[[i]]$d$X ^ 2) / nrow(models[[i]]$d))
    
    models[[i]]$t <- data.frame(X = test[[x]], Y = test[['V9']])
    
    models[[i]]$test <- list()
    models[[i]]$test$prediction <- models[[i]]$predictor(models[[i]]$t$X)
    models[[i]]$test$rss <- sum((models[[i]]$t$Y - models[[i]]$test$prediction) ^ 2)
    models[[i]]$test$tss <- var(models[[i]]$t$Y) * nrow(models[[i]]$t)
    models[[i]]$test$r2 <- 1 - models[[i]]$test$rss / models[[i]]$test$tss
    models[[i]]$test$sigma2 <- models[[i]]$test$rss / nrow(models[[i]]$t)
    models[[i]]$test$AIC <- nrow(models[[i]]$t) *
        (log(2*pi) + 1 + log(models[[i]]$test$rss / nrow(models[[i]]$t))) +
        ((2 + 1) * 2)
    
    print(sprintf('Model %d:', i))
    print(sprintf('   Model:'))
    print(sprintf('      Intercept Estimate: %0.4f', models[[i]]$intercept$estimate))
    print(sprintf('      Intercept Standard Deviation: %0.4f', models[[i]]$intercept$sd))
    print(sprintf('      Slope Estimate: %0.4f', models[[i]]$slope$estimate))
    print(sprintf('      Slope Standard Deviation: %0.4f', models[[i]]$slope$sd))
    print(sprintf('      Residual Variance: %0.4f', models[[i]]$train$sigma2))
    print(sprintf('   Train:'))
    print(sprintf('      Residual Sum of Squares: %0.4f', models[[i]]$train$rss))
    print(sprintf('      R Squared: %0.4f', models[[i]]$train$r2))
    print(sprintf('      AIC: %0.4f', models[[i]]$train$AIC))
    print(sprintf('   Test:'))
    print(sprintf('      Residual Sum of Squares: %0.4f', models[[i]]$test$rss))
    print(sprintf('      R Squared: %0.4f', models[[i]]$test$r2))
    print(sprintf('      AIC: %0.4f', models[[i]]$test$AIC))
}

models[[1]]$predictor <- function(x) {models[[1]]$intercept$estimate + models[[1]]$slope$estimate * x}
models[[1]]$plot <- models[[1]]$plot + stat_function(fun = models[[1]]$predictor, color = 'red')
models[[2]]$predictor <- function(x) {models[[2]]$intercept$estimate + models[[2]]$slope$estimate * x}
models[[2]]$plot <- models[[2]]$plot + stat_function(fun = models[[2]]$predictor, color = 'red')
models[[3]]$predictor <- function(x) {models[[3]]$intercept$estimate + models[[3]]$slope$estimate * x}
models[[3]]$plot <- models[[3]]$plot + stat_function(fun = models[[3]]$predictor, color = 'red')
models[[4]]$predictor <- function(x) {models[[4]]$intercept$estimate + models[[4]]$slope$estimate * x}
models[[4]]$plot <- models[[4]]$plot + stat_function(fun = models[[4]]$predictor, color = 'red')
models[[5]]$predictor <- function(x) {models[[5]]$intercept$estimate + models[[5]]$slope$estimate * x}
models[[5]]$plot <- models[[5]]$plot + stat_function(fun = models[[5]]$predictor, color = 'red')
models[[6]]$predictor <- function(x) {models[[6]]$intercept$estimate + models[[6]]$slope$estimate * x}
models[[6]]$plot <- models[[6]]$plot + stat_function(fun = models[[6]]$predictor, color = 'red')
models[[7]]$predictor <- function(x) {models[[7]]$intercept$estimate + models[[7]]$slope$estimate * x}
models[[7]]$plot <- models[[7]]$plot + stat_function(fun = models[[7]]$predictor, color = 'red')
models[[8]]$predictor <- function(x) {models[[8]]$intercept$estimate + models[[8]]$slope$estimate * x}
models[[8]]$plot <- models[[8]]$plot + stat_function(fun = models[[8]]$predictor, color = 'red')

# grid.arrange(models[[1]]$plot,
#              models[[2]]$plot,
#              models[[3]]$plot,
#              models[[4]]$plot,
#              models[[5]]$plot,
#              models[[6]]$plot,
#              models[[7]]$plot,
#              models[[8]]$plot,
#              nrow = 2,
#              ncol = 4)

# c) second feature
analyze_and_print <- function(model, variables) {
    model$train <- list()
    model$train$rss <- sum((model$lm$residuals) ^ 2)
    model$train$r2 <- summary(model$lm)$r.squared
    model$train$ll <- -0.5 * nrow(model$d) * (log(2*pi) + 1 + log(model$train$rss / nrow(model$d)))
    model$train$AIC <- -2 * model$train$ll + ((length(model$lm$coefficients) + 1) * 2)
    model$train$BIC <- -2 * model$train$ll + ((length(model$lm$coefficients) + 1) * log(nrow(model$d)))
    
    model$test <- list()
    model$test$prediction <- predict(model$lm, model$t)
    model$test$rss <- sum((model$t$Y - model$test$prediction) ^ 2)
    model$test$tss <- var(model$t$Y) * nrow(model$t)
    model$test$r2 <- 1 - model$test$rss / model$test$tss
    model$test$ll <- -0.5 * nrow(model$t) * (log(2*pi) + 1 + log(model$test$rss / nrow(model$t)))
    model$test$AIC <- -2 * model$test$ll + ((length(model$lm$coefficients) + 1) * 2)
    model$test$BIC <- -2 * model$test$ll + ((length(model$lm$coefficients) + 1) * log(nrow(model$t)))
    
    print(sprintf('Model %s:', variables))
    print(sprintf('   Train:'))
    print(sprintf('      Residual Sum of Squares: %0.4f', model$train$rss))
    print(sprintf('      R Squared: %0.4f', model$train$r2))
    print(sprintf('      AIC: %0.4f', model$train$AIC))
    print(sprintf('      BIC: %0.4f', model$train$BIC))
    print(sprintf('   Test:'))
    print(sprintf('      Residual Sum of Squares: %0.4f', model$test$rss))
    print(sprintf('      R Squared: %0.4f', model$test$r2))
    print(sprintf('      AIC: %0.4f', model$test$AIC))
    print(sprintf('      BIC: %0.4f', model$test$BIC))
    
    return(model)
}

print("")
print("|=========================================|")
print("|                                         |")
print("|>>>>> Part C: Select second Feature <<<<<|")
print("|                                         |")
print("|_________________________________________|")

models2 <- list()
for (i in 1:8) {
    if (i %in% c(4)) next
    
    models2[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models2[[i]]$d <- data.frame(X1 = train[['V4']], X2 = train[[x]], Y = train[['V9']])
    models2[[i]]$t <- data.frame(X1 = test[['V4']], X2 = test[[x]], Y = test[['V9']])
    models2[[i]]$lm <- lm(data = models2[[i]]$d, Y ~ X1 + X2)
    
    models2[[i]] <- analyze_and_print(models2[[i]], sprintf('4 and %d', i))
}

# d) third feature
print("")
print("|========================================|")
print("|                                        |")
print("|>>>>> Part D: Select Third Feature <<<<<|")
print("|                                        |")
print("|________________________________________|")

models3 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8)) next
    
    models3[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models3[[i]]$d <- data.frame(X1 = train[['V4']],
                                 X2 = train[['V8']],
                                 X3 = train[[x]],
                                 Y = train[['V9']])
    models3[[i]]$t <- data.frame(X1 = test[['V4']],
                                 X2 = test[['V8']],
                                 X3 = test[[x]],
                                 Y = test[['V9']])
    models3[[i]]$lm <- lm(data = models3[[i]]$d, Y ~ X1 + X2 + X3)
    
    models3[[i]] <- analyze_and_print(models3[[i]], sprintf('4, 8 and %d', i))
}

# d) fourth feature
print("")
print("|=========================================|")
print("|                                         |")
print("|>>>>> Part D: Select Fourth Feature <<<<<|")
print("|                                         |")
print("|_________________________________________|")

models4 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8, 1)) next
    
    models4[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models4[[i]]$d <- data.frame(X1 = train[['V4']],
                                 X2 = train[['V8']],
                                 X3 = train[['V1']],
                                 X4 = train[[x]],
                                 Y = train[['V9']])
    models4[[i]]$t <- data.frame(X1 = test[['V4']],
                                 X2 = test[['V8']],
                                 X3 = test[['V1']],
                                 X4 = test[[x]],
                                 Y = test[['V9']])
    models4[[i]]$lm <- lm(data = models4[[i]]$d, Y ~ X1 + X2 + X3 + X4)
    models4[[i]] <- analyze_and_print(model = models4[[i]], sprintf('4, 8, 1 and %d', i))
}

# d) fifth feature
print("")
print("|========================================|")
print("|                                        |")
print("|>>>>> Part D: Select Fifth Feature <<<<<|")
print("|                                        |")
print("|________________________________________|")

models5 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8, 1, 3)) next
    
    models5[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models5[[i]]$d <- data.frame(X1 = train[['V4']],
                                 X2 = train[['V8']],
                                 X3 = train[['V1']],
                                 X4 = train[['V3']],
                                 X5 = train[[x]],
                                 Y = train[['V9']])
    models5[[i]]$t <- data.frame(X1 = test[['V4']],
                                 X2 = test[['V8']],
                                 X3 = test[['V1']],
                                 X4 = test[['V3']],
                                 X5 = test[[x]],
                                 Y = test[['V9']])
    models5[[i]]$lm <- lm(data = models5[[i]]$d, Y ~ X1 + X2 + X3 + X4 + X5)
    models5[[i]] <- analyze_and_print(model = models5[[i]], sprintf('4, 8, 1, 3 and %d', i))
}

# d) sixth feature
print("")
print("|========================================|")
print("|                                        |")
print("|>>>>> Part D: Select Sixth Feature <<<<<|")
print("|                                        |")
print("|________________________________________|")

models6 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8, 1, 3, 5)) next
    
    models6[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models6[[i]]$d <- data.frame(X1 = train[['V4']],
                                 X2 = train[['V8']],
                                 X3 = train[['V1']],
                                 X4 = train[['V3']],
                                 X5 = train[['V5']],
                                 X6 = train[[x]],
                                 Y = train[['V9']])
    models6[[i]]$t <- data.frame(X1 = test[['V4']],
                                 X2 = test[['V8']],
                                 X3 = test[['V1']],
                                 X4 = test[['V3']],
                                 X5 = test[['V5']],
                                 X6 = test[[x]],
                                 Y = test[['V9']])
    models6[[i]]$lm <- lm(data = models6[[i]]$d, Y ~ X1 + X2 + X3 + X4 + X5 + X6)
    models6[[i]] <- analyze_and_print(model = models6[[i]], sprintf('4, 8, 1, 3, 5 and %d', i))
}

# d) seventh feature
print("")
print("|==========================================|")
print("|                                          |")
print("|>>>>> Part D: Select Seventh Feature <<<<<|")
print("|                                          |")
print("|__________________________________________|")

models7 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8, 1, 3, 5, 7)) next
    
    models7[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models7[[i]]$d <- data.frame(X1 = train[['V4']],
                                 X2 = train[['V8']],
                                 X3 = train[['V1']],
                                 X4 = train[['V3']],
                                 X5 = train[['V5']],
                                 X6 = train[['V7']],
                                 X7 = train[[x]],
                                 Y = train[['V9']])
    models7[[i]]$t <- data.frame(X1 = test[['V4']],
                                 X2 = test[['V8']],
                                 X3 = test[['V1']],
                                 X4 = test[['V3']],
                                 X5 = test[['V5']],
                                 X6 = test[['V7']],
                                 X7 = test[[x]],
                                 Y = test[['V9']])
    models7[[i]]$lm <- lm(data = models7[[i]]$d, Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7)
    models7[[i]] <- analyze_and_print(model = models7[[i]], sprintf('4, 8, 1, 3, 5, 7 and %d', i))
}

# d) eighth feature
print("")
print("|==========================================|")
print("|                                          |")
print("|>>>>> Part D: Analyze Eighth Feature <<<<<|")
print("|                                          |")
print("|__________________________________________|")

models8 <- list()
models8[[6]] <- list()

models8[[6]]$d <- data.frame(X1 = train[['V4']],
                             X2 = train[['V8']],
                             X3 = train[['V1']],
                             X4 = train[['V3']],
                             X5 = train[['V5']],
                             X6 = train[['V7']],
                             X7 = train[['V2']],
                             X8 = train[['V6']],
                             Y = train[['V9']])
models8[[6]]$t <- data.frame(X1 = test[['V4']],
                             X2 = test[['V8']],
                             X3 = test[['V1']],
                             X4 = test[['V3']],
                             X5 = test[['V5']],
                             X6 = test[['V7']],
                             X7 = test[['V2']],
                             X8 = test[['V6']],
                             Y = test[['V9']])
models8[[6]]$lm <- lm(data = models8[[6]]$d, Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8)
models8[[6]] <- analyze_and_print(model = models8[[6]], '4, 8, 1, 3, 5, 7, 2 and 6')

# d) plots
df.AIC <- data.frame(x = 1:8,
                     X = c(4, 8, 1, 3, 5, 7, 2, 6),
                     RSS_Train = c(models[[4]]$train$rss,
                                   models2[[8]]$train$rss,
                                   models3[[1]]$train$rss,
                                   models4[[3]]$train$rss,
                                   models5[[5]]$train$rss,
                                   models6[[7]]$train$rss,
                                   models7[[2]]$train$rss,
                                   models8[[6]]$train$rss),
                     RSS_Test = c(models[[4]]$test$rss,
                                  models2[[8]]$test$rss,
                                  models3[[1]]$test$rss,
                                  models4[[3]]$test$rss,
                                  models5[[5]]$test$rss,
                                  models6[[7]]$test$rss,
                                  models7[[2]]$test$rss,
                                  models8[[6]]$test$rss),
                     R2_Train = c(models[[4]]$train$r2,
                                  models2[[8]]$train$r2,
                                  models3[[1]]$train$r2,
                                  models4[[3]]$train$r2,
                                  models5[[5]]$train$r2,
                                  models6[[7]]$train$r2,
                                  models7[[2]]$train$r2,
                                  models8[[6]]$train$r2),
                     R2_Test = c(models[[4]]$test$r2,
                                 models2[[8]]$test$r2,
                                 models3[[1]]$test$r2,
                                 models4[[3]]$test$r2,
                                 models5[[5]]$test$r2,
                                 models6[[7]]$test$r2,
                                 models7[[2]]$test$r2,
                                 models8[[6]]$test$r2),
                     AIC_Train = c(models[[4]]$train$AIC,
                                   models2[[8]]$train$AIC,
                                   models3[[1]]$train$AIC,
                                   models4[[3]]$train$AIC,
                                   models5[[5]]$train$AIC,
                                   models6[[7]]$train$AIC,
                                   models7[[2]]$train$AIC,
                                   models8[[6]]$train$AIC),
                     AIC_Test = c(models[[4]]$test$AIC,
                                  models2[[8]]$test$AIC,
                                  models3[[1]]$test$AIC,
                                  models4[[3]]$test$AIC,
                                  models5[[5]]$test$AIC,
                                  models6[[7]]$test$AIC,
                                  models7[[2]]$test$AIC,
                                  models8[[6]]$test$AIC))

plot.AIC <- ggplot(data = df.AIC, aes(x = x))
plot.AIC <- plot.AIC + geom_line(aes(y = log(150 + AIC_Train), color = 'AIC of Train Set'))
plot.AIC <- plot.AIC + geom_point(aes(y = log(150 + AIC_Train), color = 'AIC of Train Set'), alpha = 0.5)
plot.AIC <- plot.AIC + geom_line(aes(y = log(150 + AIC_Test), color = 'AIC of Test Set'))
plot.AIC <- plot.AIC + geom_point(aes(y = log(150 + AIC_Test), color = 'AIC of Test Set'), alpha = 0.5)

plot.RSS <- ggplot(data = df.AIC, aes(x = x))
plot.RSS <- plot.RSS + geom_line(aes(y = log(RSS_Train), color = 'RSS of Train Set'))
plot.RSS <- plot.RSS + geom_point(aes(y = log(RSS_Train), color = 'RSS of Train Set'), alpha = 0.5)
plot.RSS <- plot.RSS + geom_line(aes(y = log(RSS_Test), color = 'RSS of Test Set'))
plot.RSS <- plot.RSS + geom_point(aes(y = log(RSS_Test), color = 'RSS of Test Set'), alpha = 0.5)

plot.R2 <- ggplot(data = df.AIC, aes(x = x))
plot.R2 <- plot.R2 + geom_line(aes(y = R2_Train, color = 'R2 of Train Set'))
plot.R2 <- plot.R2 + geom_point(aes(y = R2_Train, color = 'R2 of Train Set'), alpha = 0.5)
plot.R2 <- plot.R2 + geom_line(aes(y = R2_Test, color = 'R2 of Test Set'))
plot.R2 <- plot.R2 + geom_point(aes(y = R2_Test, color = 'R2 of Test Set'), alpha = 0.5)

# grid.arrange(plot.AIC, plot.RSS, plot.R2, ncol = 3)

# e) model selection using BIC
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part E: Select First Feature by BIC <<<<<|")
print("|                                               |")
print("|_______________________________________________|")

models.bic.1 <- list()
for (i in 1:8) {
    models.bic.1[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models.bic.1[[i]]$d <- data.frame(X1 = train[[x]],
                                      Y = train[['V9']])
    models.bic.1[[i]]$t <- data.frame(X1 = test[[x]],
                                      Y = test[['V9']])
    models.bic.1[[i]]$lm <- lm(data = models.bic.1[[i]]$d, Y ~ X1)
    models.bic.1[[i]] <- analyze_and_print(model = models.bic.1[[i]], sprintf('%d', i))
}

# e) model selection using BIC
print("")
print("|================================================|")
print("|                                                |")
print("|>>>>> Part E: Select Second Feature by BIC <<<<<|")
print("|                                                |")
print("|________________________________________________|")

models.bic.2 <- list()
for (i in 1:8) {
    if (i %in% c(4)) next
    
    models.bic.2[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models.bic.2[[i]]$d <- data.frame(X1 = train[['V4']],
                                      X2 = train[[x]],
                                      Y = train[['V9']])
    models.bic.2[[i]]$t <- data.frame(X1 = test[['V4']],
                                      X2 = test[[x]],
                                      Y = test[['V9']])
    models.bic.2[[i]]$lm <- lm(data = models.bic.2[[i]]$d, Y ~ X1 + X2)
    models.bic.2[[i]] <- analyze_and_print(model = models.bic.2[[i]], sprintf('4 and %d', i))
}

# e) model selection using BIC
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part E: Select Third Feature by BIC <<<<<|")
print("|                                               |")
print("|_______________________________________________|")

models.bic.3 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8)) next
    
    models.bic.3[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models.bic.3[[i]]$d <- data.frame(X1 = train[['V4']],
                                      X2 = train[['V8']],
                                      X3 = train[[x]],
                                      Y = train[['V9']])
    models.bic.3[[i]]$t <- data.frame(X1 = test[['V4']],
                                      X2 = test[['V8']],
                                      X3 = test[[x]],
                                      Y = test[['V9']])
    models.bic.3[[i]]$lm <- lm(data = models.bic.3[[i]]$d, Y ~ X1 + X2 + X3)
    models.bic.3[[i]] <- analyze_and_print(model = models.bic.3[[i]], sprintf('4, 8 and %d', i))
}

# e) model selection using BIC
print("")
print("|================================================|")
print("|                                                |")
print("|>>>>> Part E: Select Fourth Feature by BIC <<<<<|")
print("|                                                |")
print("|________________________________________________|")

models.bic.4 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8, 1)) next
    
    models.bic.4[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models.bic.4[[i]]$d <- data.frame(X1 = train[['V4']],
                                      X2 = train[['V8']],
                                      X3 = train[['V1']],
                                      X4 = train[[x]],
                                      Y = train[['V9']])
    models.bic.4[[i]]$t <- data.frame(X1 = test[['V4']],
                                      X2 = test[['V8']],
                                      X3 = test[['V1']],
                                      X4 = test[[x]],
                                      Y = test[['V9']])
    models.bic.4[[i]]$lm <- lm(data = models.bic.4[[i]]$d, Y ~ X1 + X2 + X3 + X4)
    models.bic.4[[i]] <- analyze_and_print(model = models.bic.4[[i]], sprintf('4, 8, 1 and %d', i))
}

# e) model selection using BIC
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part E: Select Fifth Feature by BIC <<<<<|")
print("|                                               |")
print("|_______________________________________________|")

models.bic.5 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8, 1, 3)) next
    
    models.bic.5[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models.bic.5[[i]]$d <- data.frame(X1 = train[['V4']],
                                      X2 = train[['V8']],
                                      X3 = train[['V1']],
                                      X4 = train[['V3']],
                                      X5 = train[[x]],
                                      Y = train[['V9']])
    models.bic.5[[i]]$t <- data.frame(X1 = test[['V4']],
                                      X2 = test[['V8']],
                                      X3 = test[['V1']],
                                      X4 = test[['V3']],
                                      X5 = test[[x]],
                                      Y = test[['V9']])
    models.bic.5[[i]]$lm <- lm(data = models.bic.5[[i]]$d, Y ~ X1 + X2 + X3 + X4 + X5)
    models.bic.5[[i]] <- analyze_and_print(model = models.bic.5[[i]], sprintf('4, 8, 1, 3 and %d', i))
}

# e) model selection using BIC
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part E: Select Sixth Feature by BIC <<<<<|")
print("|                                               |")
print("|_______________________________________________|")

models.bic.6 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8, 1, 3, 5)) next
    
    models.bic.6[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models.bic.6[[i]]$d <- data.frame(X1 = train[['V4']],
                                      X2 = train[['V8']],
                                      X3 = train[['V1']],
                                      X4 = train[['V3']],
                                      X5 = train[['V5']],
                                      X6 = train[[x]],
                                      Y = train[['V9']])
    models.bic.6[[i]]$t <- data.frame(X1 = test[['V4']],
                                      X2 = test[['V8']],
                                      X3 = test[['V1']],
                                      X4 = test[['V3']],
                                      X5 = test[['V5']],
                                      X6 = test[[x]],
                                      Y = test[['V9']])
    models.bic.6[[i]]$lm <- lm(data = models.bic.6[[i]]$d, Y ~ X1 + X2 + X3 + X4 + X5 + X6)
    models.bic.6[[i]] <- analyze_and_print(model = models.bic.6[[i]], sprintf('4, 8, 1, 3, 5 and %d', i))
}

# e) model selection using BIC
print("")
print("|=================================================|")
print("|                                                 |")
print("|>>>>> Part E: Select Seventh Feature by BIC <<<<<|")
print("|                                                 |")
print("|_________________________________________________|")

models.bic.7 <- list()
for (i in 1:8) {
    if (i %in% c(4, 8, 1, 3, 5, 7)) next
    
    models.bic.7[[i]] <- list()
    
    x <- sprintf('V%d', i)
    models.bic.7[[i]]$d <- data.frame(X1 = train[['V4']],
                                      X2 = train[['V8']],
                                      X3 = train[['V1']],
                                      X4 = train[['V3']],
                                      X5 = train[['V5']],
                                      X6 = train[['V7']],
                                      X7 = train[[x]],
                                      Y = train[['V9']])
    models.bic.7[[i]]$t <- data.frame(X1 = test[['V4']],
                                      X2 = test[['V8']],
                                      X3 = test[['V1']],
                                      X4 = test[['V3']],
                                      X5 = test[['V5']],
                                      X6 = test[['V7']],
                                      X7 = test[[x]],
                                      Y = test[['V9']])
    models.bic.7[[i]]$lm <- lm(data = models.bic.7[[i]]$d, Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7)
    models.bic.7[[i]] <- analyze_and_print(model = models.bic.7[[i]], sprintf('4, 8, 1, 3, 5, 7 and %d', i))
}

# e) model selection using BIC
print("")
print("|================================================|")
print("|                                                |")
print("|>>>>> Part E: Select Eighth Feature by BIC <<<<<|")
print("|                                                |")
print("|________________________________________________|")

models.bic.8 <- list()
models.bic.8[[6]] <- list()
models.bic.8[[6]]$d <- data.frame(X1 = train[['V4']],
                                  X2 = train[['V8']],
                                  X3 = train[['V1']],
                                  X4 = train[['V3']],
                                  X5 = train[['V5']],
                                  X6 = train[['V7']],
                                  X7 = train[['V2']],
                                  X8 = train[['V6']],
                                  Y = train[['V9']])
models.bic.8[[6]]$t <- data.frame(X1 = test[['V4']],
                                  X2 = test[['V8']],
                                  X3 = test[['V1']],
                                  X4 = test[['V3']],
                                  X5 = test[['V5']],
                                  X6 = test[['V7']],
                                  X7 = test[['V2']],
                                  X8 = test[['V6']],
                                  Y = test[['V9']])
models.bic.8[[6]]$lm <- lm(data = models.bic.8[[6]]$d, Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8)
models.bic.8[[6]] <- analyze_and_print(model = models.bic.8[[6]], '4, 8, 1, 3, 5, 7, 2 and 6')

# e) plots
df.BIC <- data.frame(x = 1:8,
                     X = c(4, 8, 1, 3, 5, 7, 2, 6),
                     RSS_Train = c(models.bic.1[[4]]$train$rss,
                                   models.bic.2[[8]]$train$rss,
                                   models.bic.3[[1]]$train$rss,
                                   models.bic.4[[3]]$train$rss,
                                   models.bic.5[[5]]$train$rss,
                                   models.bic.6[[7]]$train$rss,
                                   models.bic.7[[2]]$train$rss,
                                   models.bic.8[[6]]$train$rss),
                     RSS_Test = c(models.bic.1[[4]]$test$rss,
                                  models.bic.2[[8]]$test$rss,
                                  models.bic.3[[1]]$test$rss,
                                  models.bic.4[[3]]$test$rss,
                                  models.bic.5[[5]]$test$rss,
                                  models.bic.6[[7]]$test$rss,
                                  models.bic.7[[2]]$test$rss,
                                  models.bic.8[[6]]$test$rss),
                     R2_Train = c(models.bic.1[[4]]$train$r2,
                                  models.bic.2[[8]]$train$r2,
                                  models.bic.3[[1]]$train$r2,
                                  models.bic.4[[3]]$train$r2,
                                  models.bic.5[[5]]$train$r2,
                                  models.bic.6[[7]]$train$r2,
                                  models.bic.7[[2]]$train$r2,
                                  models.bic.8[[6]]$train$r2),
                     R2_Test = c(models.bic.1[[4]]$test$r2,
                                 models.bic.2[[8]]$test$r2,
                                 models.bic.3[[1]]$test$r2,
                                 models.bic.4[[3]]$test$r2,
                                 models.bic.5[[5]]$test$r2,
                                 models.bic.6[[7]]$test$r2,
                                 models.bic.7[[2]]$test$r2,
                                 models.bic.8[[6]]$test$r2),
                     BIC_Train = c(models.bic.1[[4]]$train$BIC,
                                   models.bic.2[[8]]$train$BIC,
                                   models.bic.3[[1]]$train$BIC,
                                   models.bic.4[[3]]$train$BIC,
                                   models.bic.5[[5]]$train$BIC,
                                   models.bic.6[[7]]$train$BIC,
                                   models.bic.7[[2]]$train$BIC,
                                   models.bic.8[[6]]$train$BIC),
                     BIC_Test = c(models.bic.1[[4]]$test$BIC,
                                  models.bic.2[[8]]$test$BIC,
                                  models.bic.3[[1]]$test$BIC,
                                  models.bic.4[[3]]$test$BIC,
                                  models.bic.5[[5]]$test$BIC,
                                  models.bic.6[[7]]$test$BIC,
                                  models.bic.7[[2]]$test$BIC,
                                  models.bic.8[[6]]$test$BIC))

plot.BIC.BIC <- ggplot(data = df.BIC, aes(x = x))
plot.BIC.BIC <- plot.BIC.BIC + geom_line(aes(y = log(150 + BIC_Train), color = 'BIC of Train Set'))
plot.BIC.BIC <- plot.BIC.BIC + geom_point(aes(y = log(150 + BIC_Train), color = 'BIC of Train Set'), alpha = 0.5)
plot.BIC.BIC <- plot.BIC.BIC + geom_line(aes(y = log(150 + BIC_Test), color = 'BIC of Test Set'))
plot.BIC.BIC <- plot.BIC.BIC + geom_point(aes(y = log(150 + BIC_Test), color = 'BIC of Test Set'), alpha = 0.5)

plot.BIC.RSS <- ggplot(data = df.BIC, aes(x = x))
plot.BIC.RSS <- plot.BIC.RSS + geom_line(aes(y = log(RSS_Train), color = 'RSS of Train Set'))
plot.BIC.RSS <- plot.BIC.RSS + geom_point(aes(y = log(RSS_Train), color = 'RSS of Train Set'), alpha = 0.5)
plot.BIC.RSS <- plot.BIC.RSS + geom_line(aes(y = log(RSS_Test), color = 'RSS of Test Set'))
plot.BIC.RSS <- plot.BIC.RSS + geom_point(aes(y = log(RSS_Test), color = 'RSS of Test Set'), alpha = 0.5)

plot.BIC.R2 <- ggplot(data = df.BIC, aes(x = x))
plot.BIC.R2 <- plot.BIC.R2 + geom_line(aes(y = R2_Train, color = 'R2 of Train Set'))
plot.BIC.R2 <- plot.BIC.R2 + geom_point(aes(y = R2_Train, color = 'R2 of Train Set'), alpha = 0.5)
plot.BIC.R2 <- plot.BIC.R2 + geom_line(aes(y = R2_Test, color = 'R2 of Test Set'))
plot.BIC.R2 <- plot.BIC.R2 + geom_point(aes(y = R2_Test, color = 'R2 of Test Set'), alpha = 0.5)

# grid.arrange(plot.BIC.BIC, plot.BIC.RSS, plot.BIC.R2, ncol = 3)

df.AIC.BIC = data.frame(x = df.AIC$x,
                        X = df.AIC$X,
                        AIC_Train = df.AIC$AIC_Train,
                        AIC_Test = df.AIC$AIC_Test,
                        BIC_Train = df.BIC$BIC_Train,
                        BIC_Test = df.BIC$BIC_Test);
plot.AIC.BIC <- ggplot(data = df.AIC.BIC, aes(x = x))
plot.AIC.BIC <- plot.AIC.BIC + geom_line(aes(y = log(150 + AIC_Train), color = 'AIC of Train Set'))
plot.AIC.BIC <- plot.AIC.BIC + geom_point(aes(y = log(150 + AIC_Train), color = 'AIC of Train Set'), alpha = 0.5)
plot.AIC.BIC <- plot.AIC.BIC + geom_line(aes(y = log(150 + AIC_Test), color = 'AIC of Test Set'))
plot.AIC.BIC <- plot.AIC.BIC + geom_point(aes(y = log(150 + AIC_Test), color = 'AIC of Test Set'), alpha = 0.5)
plot.AIC.BIC <- plot.AIC.BIC + geom_line(aes(y = log(150 + BIC_Train), color = 'BIC of Train Set'))
plot.AIC.BIC <- plot.AIC.BIC + geom_point(aes(y = log(150 + BIC_Train), color = 'BIC of Train Set'), alpha = 0.5)
plot.AIC.BIC <- plot.AIC.BIC + geom_line(aes(y = log(150 + BIC_Test), color = 'BIC of Test Set'))
plot.AIC.BIC <- plot.AIC.BIC + geom_point(aes(y = log(150 + BIC_Test), color = 'BIC of Test Set'), alpha = 0.5)

show(plot.AIC.BIC)


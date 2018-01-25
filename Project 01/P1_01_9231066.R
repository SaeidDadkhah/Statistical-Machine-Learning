if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')
if (!'reshape2' %in% rownames(installed.packages())) {install.packages('reshape2')}
library('reshape2')

# delete old data
rm(list = ls())
set.seed(9231066)

# functions
measure <- function(y, y.hat, complexity) {
    stats <- list()
    
    mean.tmp <- mean(y)
    
    stats[['residuals']] <- y - y.hat
    stats[['rss']] <- sum(stats$residuals ^ 2)
    stats[['sigma2']] <- sum(stats$residuals ^ 2) / nrow(y)
    stats[['UnbiasedSigma2']] <- sum(stats$residuals ^ 2) / (nrow(y) - complexity)
    stats[['r2']] <- 1 - stats$rss / sum((y - mean.tmp) ^ 2)
    stats[['ll']] <- -0.5 * nrow(y) * (log(2*pi) + 1 + log(stats$rss / nrow(y)))
    stats[['AIC']] <- -2 * stats$ll + ((complexity + 1) * 2)
    stats[['BIC']] <- -2 * stats$ll + ((complexity + 1) * log(nrow(y)))
    
    stats
}

# linear model
myLM.predict <- function(model, X) {
    X <- cbind(
        matrix(1, nrow = nrow(X), ncol = 1),
        X
    )
    
    X %*% model$beta
}

myLM.train <- function(X.train, y.train, X.test, y.test) {
    X.train <- cbind(
        matrix(1, nrow = nrow(X.train), ncol = 1),
        X.train
    )
    if (is.null(colnames(X.train))) {
        colnames(X.train) <- c('(Intercept)', 'First Feature')
    }
    colnames(X.train)[1] <- '(Intercept)'
    
    model <- list()
    model[['beta']] <- solve(t(X.train) %*% X.train) %*% t(X.train) %*% y.train
    
    model[['train']] <- measure(y = y.train,
                                y.hat = X.train %*% model$beta,
                                complexity = nrow(model$beta))
    model[['test']] <- measure(y = y.test,
                               y.hat = myLM.predict(model, X.test),
                               complexity = nrow(model$beta))
    
    model[['CovMat']] <- model$train$UnbiasedSigma2 * solve(t(X.train) %*% X.train)
    
    Xs <- data.matrix(X.train)
    U <- Xs %*% solve(t(Xs) %*% Xs) %*% t(Xs)
    model[['L1O']] <- sum(model$train$residuals ^ 2 / (1 - diag(U)))
    
    model
}

# Rcv
cv.l1o.n <- function(train, test, feature.columns) {
    r.cv.n <- 0
    for (i in 1:nrow(train)) {
        m <- myLM.train(data.matrix(train[-i, feature.columns]),
                        data.matrix(train[-i, 'V9']),
                        data.matrix(test[, feature.columns]),
                        data.matrix(test[, 'V9']))
        y.hat <- myLM.predict(m, data.matrix(train[i, feature.columns]))
        r.cv.n <- r.cv.n + (train[i, 'V9'] - y.hat) ^ 2
    }
    
    r.cv.n
}

# base function
printAnalysis <- function(model, variables) {
    print(sprintf('Model %s:', variables))
    print(sprintf('   Rcv(L1O): %0.4f', model$L1O))
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
}

addOthersAndAnalyze <- function(data.train, data.test, fixedColumns) {
    models <- list()
    fixedColumns.text <- c()
    for (i in fixedColumns) {
        fixedColumns.text <- c(fixedColumns.text, sprintf('V%d', i))
    }
    for (i in 1:8) {
        if (i %in% fixedColumns) next
        
        models[[i]] <- list()
        
        x <- sprintf('V%d', i)
        models[[i]] <- myLM.train(data.matrix(data.train[, c(fixedColumns.text, x)]),
                                  data.matrix(data.train[, c('V9')]),
                                  data.matrix(data.test[, c(fixedColumns.text, x)]),
                                  data.matrix(data.test[, c('V9')]))
        
        printAnalysis(models[[i]], sprintf('%s and %d', paste(fixedColumns[i], collapse = ", "), i))
    }
    
    models
}

removeOneAndAnalyze <- function(data.train, data.test, allColumns) {
    models <- list()
    allColumns.text <- c()
    for (i in allColumns) {
        allColumns.text <- c(allColumns.text, sprintf('V%d', i))
    }
    for (i in 1:length(allColumns)) {
        models[[allColumns.text[[i]]]] <- list()
        currentColumns.text <- allColumns.text[-i]
        
        models[[allColumns.text[[i]]]] <- myLM.train(data.matrix(data.train[, currentColumns.text]),
                                                     data.matrix(data.train[, c('V9')]),
                                                     data.matrix(data.test[, currentColumns.text]),
                                                     data.matrix(data.test[, c('V9')]))
        
        printAnalysis(models[[allColumns.text[[i]]]], sprintf('%s', paste(currentColumns.text, collapse = ", ")))
    }
    
    models
}

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
    models[[i]]$t <- data.frame(X = test[[x]], Y = test[['V9']])
    
    models[[i]]$slope <- list()
    models[[i]]$intercept <- list()
    models[[i]]$slope$estimate <- cov(models[[i]]$d$X, models[[i]]$d$Y) / var(models[[i]]$d$X)
    models[[i]]$intercept$estimate <- mean(models[[i]]$d$Y) - models[[i]]$slope$estimate * mean(models[[i]]$d$X)
    models[[i]]$predictor <- function(x) {models[[i]]$intercept$estimate + models[[i]]$slope$estimate * x}
    
    models[[i]]$plot <- ggplot(data = models[[i]]$d, aes(x = X, y = Y))
    models[[i]]$plot <- models[[i]]$plot + geom_point(color = '#56B4E9')
    
    models[[i]]$train <- measure(data.matrix(models[[i]]$d$Y), data.matrix(models[[i]]$predictor(models[[i]]$d$X)), 2)
    models[[i]]$test <- measure(data.matrix(models[[i]]$t$Y), data.matrix(models[[i]]$predictor(models[[i]]$t$X)), 2)
    
    m <- mean(models[[i]]$d$X)
    models[[i]]$slope$sd <- sqrt(models[[i]]$train$sigma2 / sum((models[[i]]$d$X - m) ^ 2))
    models[[i]]$intercept$sd <- sqrt(models[[i]]$train$sigma2 / sum((models[[i]]$d$X - m) ^ 2)) * 
        sqrt(sum(models[[i]]$d$X ^ 2) / nrow(models[[i]]$d))
    
    
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

m <- models
models <- list()
models$AIC <- list()
models$AIC[[1]] <- m

models$AIC[[1]][[1]]$predictor <- function(x) {models$AIC[[1]][[1]]$intercept$estimate + models$AIC[[1]][[1]]$slope$estimate * x}
models$AIC[[1]][[1]]$plot <- models$AIC[[1]][[1]]$plot + stat_function(fun = models$AIC[[1]][[1]]$predictor, color = 'red')
models$AIC[[1]][[2]]$predictor <- function(x) {models$AIC[[1]][[2]]$intercept$estimate + models$AIC[[1]][[2]]$slope$estimate * x}
models$AIC[[1]][[2]]$plot <- models$AIC[[1]][[2]]$plot + stat_function(fun = models$AIC[[1]][[2]]$predictor, color = 'red')
models$AIC[[1]][[3]]$predictor <- function(x) {models$AIC[[1]][[3]]$intercept$estimate + models$AIC[[1]][[3]]$slope$estimate * x}
models$AIC[[1]][[3]]$plot <- models$AIC[[1]][[3]]$plot + stat_function(fun = models$AIC[[1]][[3]]$predictor, color = 'red')
models$AIC[[1]][[4]]$predictor <- function(x) {models$AIC[[1]][[4]]$intercept$estimate + models$AIC[[1]][[4]]$slope$estimate * x}
models$AIC[[1]][[4]]$plot <- models$AIC[[1]][[4]]$plot + stat_function(fun = models$AIC[[1]][[4]]$predictor, color = 'red')
models$AIC[[1]][[5]]$predictor <- function(x) {models$AIC[[1]][[5]]$intercept$estimate + models$AIC[[1]][[5]]$slope$estimate * x}
models$AIC[[1]][[5]]$plot <- models$AIC[[1]][[5]]$plot + stat_function(fun = models$AIC[[1]][[5]]$predictor, color = 'red')
models$AIC[[1]][[6]]$predictor <- function(x) {models$AIC[[1]][[6]]$intercept$estimate + models$AIC[[1]][[6]]$slope$estimate * x}
models$AIC[[1]][[6]]$plot <- models$AIC[[1]][[6]]$plot + stat_function(fun = models$AIC[[1]][[6]]$predictor, color = 'red')
models$AIC[[1]][[7]]$predictor <- function(x) {models$AIC[[1]][[7]]$intercept$estimate + models$AIC[[1]][[7]]$slope$estimate * x}
models$AIC[[1]][[7]]$plot <- models$AIC[[1]][[7]]$plot + stat_function(fun = models$AIC[[1]][[7]]$predictor, color = 'red')
models$AIC[[1]][[8]]$predictor <- function(x) {models$AIC[[1]][[8]]$intercept$estimate + models$AIC[[1]][[8]]$slope$estimate * x}
models$AIC[[1]][[8]]$plot <- models$AIC[[1]][[8]]$plot + stat_function(fun = models$AIC[[1]][[8]]$predictor, color = 'red')

# grid.arrange(models$AIC[[1]][[1]]$plot,
#              models$AIC[[1]][[2]]$plot,
#              models$AIC[[1]][[3]]$plot,
#              models$AIC[[1]][[4]]$plot,
#              models$AIC[[1]][[5]]$plot,
#              models$AIC[[1]][[6]]$plot,
#              models$AIC[[1]][[7]]$plot,
#              models$AIC[[1]][[8]]$plot,
#              nrow = 2,
#              ncol = 4)

# c) second feature
print("")
print("|=========================================|")
print("|                                         |")
print("|>>>>> Part C: Select second Feature <<<<<|")
print("|                                         |")
print("|_________________________________________|")

models$AIC[[2]] <- addOthersAndAnalyze(train, test, c(4))

# d) third feature
print("")
print("|========================================|")
print("|                                        |")
print("|>>>>> Part D: Select Third Feature <<<<<|")
print("|                                        |")
print("|________________________________________|")

models$AIC[[3]] <- addOthersAndAnalyze(train, test, c(4, 8))

# d) fourth feature
print("")
print("|=========================================|")
print("|                                         |")
print("|>>>>> Part D: Select Fourth Feature <<<<<|")
print("|                                         |")
print("|_________________________________________|")

models$AIC[[4]] <- addOthersAndAnalyze(train, test, c(4, 8, 1))

# d) fifth feature
print("")
print("|========================================|")
print("|                                        |")
print("|>>>>> Part D: Select Fifth Feature <<<<<|")
print("|                                        |")
print("|________________________________________|")

models$AIC[[5]] <- addOthersAndAnalyze(train, test, c(4, 8, 1, 3))

# d) sixth feature
print("")
print("|========================================|")
print("|                                        |")
print("|>>>>> Part D: Select Sixth Feature <<<<<|")
print("|                                        |")
print("|________________________________________|")

models$AIC[[6]] <- addOthersAndAnalyze(train, test, c(4, 8, 1, 3, 5))

# d) seventh feature
print("")
print("|==========================================|")
print("|                                          |")
print("|>>>>> Part D: Select Seventh Feature <<<<<|")
print("|                                          |")
print("|__________________________________________|")

models$AIC[[7]] <- addOthersAndAnalyze(train, test, c(4, 8, 1, 3, 5, 7))

# d) eighth feature
print("")
print("|==========================================|")
print("|                                          |")
print("|>>>>> Part D: Analyze Eighth Feature <<<<<|")
print("|                                          |")
print("|__________________________________________|")

models$AIC[[8]] <- list()
models$AIC[[8]][[6]] <-  myLM.train(data.matrix(train[, c('V4', 'V8', 'V1', 'V3', 'V5', 'V7', 'V2', 'V6')]),
                                 data.matrix(train[, c('V9')]),
                                 data.matrix(test[, c('V4', 'V8', 'V1', 'V3', 'V5', 'V7', 'V2', 'V6')]),
                                 data.matrix(test[, c('V9')]))
printAnalysis(models$AIC[[8]][[6]], '4, 8, 1, 3, 5, 7, 2 and 6')

# d) plots
df.AIC <- data.frame(x = 1:8,
                     X = c(4, 8, 1, 3, 5, 7, 2, 6),
                     RSS_Train = c(models$AIC[[1]][[4]]$train$rss,
                                   models$AIC[[2]][[8]]$train$rss,
                                   models$AIC[[3]][[1]]$train$rss,
                                   models$AIC[[4]][[3]]$train$rss,
                                   models$AIC[[5]][[5]]$train$rss,
                                   models$AIC[[6]][[7]]$train$rss,
                                   models$AIC[[7]][[2]]$train$rss,
                                   models$AIC[[8]][[6]]$train$rss),
                     RSS_Test = c(models$AIC[[1]][[4]]$test$rss,
                                  models$AIC[[2]][[8]]$test$rss,
                                  models$AIC[[3]][[1]]$test$rss,
                                  models$AIC[[4]][[3]]$test$rss,
                                  models$AIC[[5]][[5]]$test$rss,
                                  models$AIC[[6]][[7]]$test$rss,
                                  models$AIC[[7]][[2]]$test$rss,
                                  models$AIC[[8]][[6]]$test$rss),
                     R2_Train = c(models$AIC[[1]][[4]]$train$r2,
                                  models$AIC[[2]][[8]]$train$r2,
                                  models$AIC[[3]][[1]]$train$r2,
                                  models$AIC[[4]][[3]]$train$r2,
                                  models$AIC[[5]][[5]]$train$r2,
                                  models$AIC[[6]][[7]]$train$r2,
                                  models$AIC[[7]][[2]]$train$r2,
                                  models$AIC[[8]][[6]]$train$r2),
                     R2_Test = c(models$AIC[[1]][[4]]$test$r2,
                                 models$AIC[[2]][[8]]$test$r2,
                                 models$AIC[[3]][[1]]$test$r2,
                                 models$AIC[[4]][[3]]$test$r2,
                                 models$AIC[[5]][[5]]$test$r2,
                                 models$AIC[[6]][[7]]$test$r2,
                                 models$AIC[[7]][[2]]$test$r2,
                                 models$AIC[[8]][[6]]$test$r2),
                     AIC_Train = c(models$AIC[[1]][[4]]$train$AIC,
                                   models$AIC[[2]][[8]]$train$AIC,
                                   models$AIC[[3]][[1]]$train$AIC,
                                   models$AIC[[4]][[3]]$train$AIC,
                                   models$AIC[[5]][[5]]$train$AIC,
                                   models$AIC[[6]][[7]]$train$AIC,
                                   models$AIC[[7]][[2]]$train$AIC,
                                   models$AIC[[8]][[6]]$train$AIC),
                     AIC_Test = c(models$AIC[[1]][[4]]$test$AIC,
                                  models$AIC[[2]][[8]]$test$AIC,
                                  models$AIC[[3]][[1]]$test$AIC,
                                  models$AIC[[4]][[3]]$test$AIC,
                                  models$AIC[[5]][[5]]$test$AIC,
                                  models$AIC[[6]][[7]]$test$AIC,
                                  models$AIC[[7]][[2]]$test$AIC,
                                  models$AIC[[8]][[6]]$test$AIC))

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

models$BIC <- list()
models$BIC[[1]] <- addOthersAndAnalyze(train, test, c())

# e) model selection using BIC
print("")
print("|================================================|")
print("|                                                |")
print("|>>>>> Part E: Select Second Feature by BIC <<<<<|")
print("|                                                |")
print("|________________________________________________|")

models$BIC[[2]] <- addOthersAndAnalyze(train, test, c(4))

# e) model selection using BIC
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part E: Select Third Feature by BIC <<<<<|")
print("|                                               |")
print("|_______________________________________________|")

models$BIC[[3]] <- addOthersAndAnalyze(train, test, c(4, 8))

# e) model selection using BIC
print("")
print("|================================================|")
print("|                                                |")
print("|>>>>> Part E: Select Fourth Feature by BIC <<<<<|")
print("|                                                |")
print("|________________________________________________|")

models$BIC[[4]] <- addOthersAndAnalyze(train, test, c(4, 8, 1))

# e) model selection using BIC
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part E: Select Fifth Feature by BIC <<<<<|")
print("|                                               |")
print("|_______________________________________________|")

models$BIC[[5]] <- addOthersAndAnalyze(train, test, c(4, 8, 1, 3))

# e) model selection using BIC
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part E: Select Sixth Feature by BIC <<<<<|")
print("|                                               |")
print("|_______________________________________________|")

models$BIC[[6]] <- addOthersAndAnalyze(train, test, c(4, 8, 1, 3, 5))

# e) model selection using BIC
print("")
print("|=================================================|")
print("|                                                 |")
print("|>>>>> Part E: Select Seventh Feature by BIC <<<<<|")
print("|                                                 |")
print("|_________________________________________________|")

models$BIC[[7]] <- addOthersAndAnalyze(train, test, c(4, 8, 1, 3, 5, 7))

# e) model selection using BIC
print("")
print("|================================================|")
print("|                                                |")
print("|>>>>> Part E: Select Eighth Feature by BIC <<<<<|")
print("|                                                |")
print("|________________________________________________|")

models$BIC[[8]] <- list()
models$BIC[[8]][[6]] <-  myLM.train(data.matrix(train[, c('V4', 'V8', 'V1', 'V3', 'V5', 'V7', 'V2', 'V6')]),
                                    data.matrix(train[, c('V9')]),
                                    data.matrix(test[, c('V4', 'V8', 'V1', 'V3', 'V5', 'V7', 'V2', 'V6')]),
                                    data.matrix(test[, c('V9')]))
printAnalysis(models$BIC[[8]][[6]], '4, 8, 1, 3, 5, 7, 2 and 6')

# e) plots
df.BIC <- data.frame(x = 1:8,
                     X = c(4, 8, 1, 3, 5, 7, 2, 6),
                     RSS_Train = c(models$BIC[[1]][[4]]$train$rss,
                                   models$BIC[[2]][[8]]$train$rss,
                                   models$BIC[[3]][[1]]$train$rss,
                                   models$BIC[[4]][[3]]$train$rss,
                                   models$BIC[[5]][[5]]$train$rss,
                                   models$BIC[[6]][[7]]$train$rss,
                                   models$BIC[[7]][[2]]$train$rss,
                                   models$BIC[[8]][[6]]$train$rss),
                     RSS_Test = c(models$BIC[[1]][[4]]$test$rss,
                                  models$BIC[[2]][[8]]$test$rss,
                                  models$BIC[[3]][[1]]$test$rss,
                                  models$BIC[[4]][[3]]$test$rss,
                                  models$BIC[[5]][[5]]$test$rss,
                                  models$BIC[[6]][[7]]$test$rss,
                                  models$BIC[[7]][[2]]$test$rss,
                                  models$BIC[[8]][[6]]$test$rss),
                     R2_Train = c(models$BIC[[1]][[4]]$train$r2,
                                  models$BIC[[2]][[8]]$train$r2,
                                  models$BIC[[3]][[1]]$train$r2,
                                  models$BIC[[4]][[3]]$train$r2,
                                  models$BIC[[5]][[5]]$train$r2,
                                  models$BIC[[6]][[7]]$train$r2,
                                  models$BIC[[7]][[2]]$train$r2,
                                  models$BIC[[8]][[6]]$train$r2),
                     R2_Test = c(models$BIC[[1]][[4]]$test$r2,
                                 models$BIC[[2]][[8]]$test$r2,
                                 models$BIC[[3]][[1]]$test$r2,
                                 models$BIC[[4]][[3]]$test$r2,
                                 models$BIC[[5]][[5]]$test$r2,
                                 models$BIC[[6]][[7]]$test$r2,
                                 models$BIC[[7]][[2]]$test$r2,
                                 models$BIC[[8]][[6]]$test$r2),
                     BIC_Train = c(models$BIC[[1]][[4]]$train$BIC,
                                   models$BIC[[2]][[8]]$train$BIC,
                                   models$BIC[[3]][[1]]$train$BIC,
                                   models$BIC[[4]][[3]]$train$BIC,
                                   models$BIC[[5]][[5]]$train$BIC,
                                   models$BIC[[6]][[7]]$train$BIC,
                                   models$BIC[[7]][[2]]$train$BIC,
                                   models$BIC[[8]][[6]]$train$BIC),
                     BIC_Test = c(models$BIC[[1]][[4]]$test$BIC,
                                  models$BIC[[2]][[8]]$test$BIC,
                                  models$BIC[[3]][[1]]$test$BIC,
                                  models$BIC[[4]][[3]]$test$BIC,
                                  models$BIC[[5]][[5]]$test$BIC,
                                  models$BIC[[6]][[7]]$test$BIC,
                                  models$BIC[[7]][[2]]$test$BIC,
                                  models$BIC[[8]][[6]]$test$BIC))

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

# show(plot.AIC.BIC)

# f)
model <- models$AIC[[8]][[6]]
print(model$CovMat)
print(sprintf('Unbiased Sigma^2: %f', model$train$UnbiasedSigma2))
cnames <- colnames(model$CovMat)
for (i in 1:9) {
    print(sprintf('Beta(%-11s) = %10.5f ± %10.5f', cnames[i], model$beta[i], model$CovMat[i, i]))
}

feature.columns <- c()
for (i in 1:8) {
    feature.columns <- c(feature.columns, sprintf('V%d', i))
}

# r.cv.l1o.1 <- cv.l1o.n(train, test, feature.columns)

print('Rcv:')
print(sprintf('   1 training: %f', model$L1O))
# print(sprintf('   n training: %f', r.cv.l1o.1))

# g) model selection using L1O
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part G: Remove First Feature by L1O <<<<<|")
print("|                                               |")
print("|_______________________________________________|")
models$L1O <- list()
models$L1O[[8]] <- removeOneAndAnalyze(train, test, c(1, 2, 3, 4, 5, 6, 7, 8))

# g) model selection using L1O
print("")
print("|================================================|")
print("|                                                |")
print("|>>>>> Part G: Remove Second Feature by L1O <<<<<|")
print("|                                                |")
print("|________________________________________________|")
models$L1O[[7]] <- removeOneAndAnalyze(train, test, c(1, 2, 3, 4, 5, 7, 8))

# g) model selection using L1O
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part G: Remove Third Feature by L1O <<<<<|")
print("|                                               |")
print("|_______________________________________________|")
models$L1O[[6]] <- removeOneAndAnalyze(train, test, c(1, 3, 4, 5, 7, 8))

# g) model selection using L1O
print("")
print("|================================================|")
print("|                                                |")
print("|>>>>> Part G: Remove Fourth Feature by L1O <<<<<|")
print("|                                                |")
print("|________________________________________________|")
models$L1O[[5]] <- removeOneAndAnalyze(train, test, c(3, 4, 5, 7, 8))

# g) model selection using L1O
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part G: Remove Fifth Feature by L1O <<<<<|")
print("|                                               |")
print("|_______________________________________________|")
models$L1O[[4]] <- removeOneAndAnalyze(train, test, c(3, 4, 7, 8))

# g) model selection using L1O
print("")
print("|===============================================|")
print("|                                               |")
print("|>>>>> Part G: Remove Sixth Feature by L1O <<<<<|")
print("|                                               |")
print("|_______________________________________________|")
models$L1O[[3]] <- removeOneAndAnalyze(train, test, c(4, 7, 8))

# g) model selection using L1O
print("")
print("|=================================================|")
print("|                                                 |")
print("|>>>>> Part G: Remove Seventh Feature by L1O <<<<<|")
print("|                                                 |")
print("|_________________________________________________|")
models$L1O[[2]] <- removeOneAndAnalyze(train, test, c(4, 8))

# g) plots
df.L1O <- data.frame(x = 1:7,
                     X = c(6, 2, 1, 5, 3, 7, 8),
                     L1O = c(models$L1O[[8]]$V6$L1O,
                             models$L1O[[7]]$V2$L1O,
                             models$L1O[[6]]$V1$L1O,
                             models$L1O[[5]]$V5$L1O,
                             models$L1O[[4]]$V3$L1O,
                             models$L1O[[3]]$V7$L1O,
                             models$L1O[[2]]$V8$L1O),
                     RSS_Train = c(models$L1O[[8]]$V6$train$rss,
                                   models$L1O[[7]]$V2$train$rss,
                                   models$L1O[[6]]$V1$train$rss,
                                   models$L1O[[5]]$V5$train$rss,
                                   models$L1O[[4]]$V3$train$rss,
                                   models$L1O[[3]]$V7$train$rss,
                                   models$L1O[[2]]$V8$train$rss),
                     RSS_Test = c(models$L1O[[8]]$V6$test$rss,
                                  models$L1O[[7]]$V2$test$rss,
                                  models$L1O[[6]]$V1$test$rss,
                                  models$L1O[[5]]$V5$test$rss,
                                  models$L1O[[4]]$V3$test$rss,
                                  models$L1O[[3]]$V7$test$rss,
                                  models$L1O[[2]]$V8$test$rss))

plot.L1O <- ggplot(data = df.L1O, aes(x = x))
plot.L1O <- plot.L1O + geom_line(aes(y = log(L1O), color = 'Rcv L1O'))
plot.L1O <- plot.L1O + geom_point(aes(y = log(L1O), color = 'Rcv L1O'), alpha = 0.5)
plot.L1O <- plot.L1O + geom_line(aes(y = log(RSS_Train), color = 'RSS of Train Set'))
plot.L1O <- plot.L1O + geom_point(aes(y = log(RSS_Train), color = 'RSS of Train Set'), alpha = 0.5)
plot.L1O <- plot.L1O + geom_line(aes(y = log(RSS_Test), color = 'RSS of Test Set'))
plot.L1O <- plot.L1O + geom_point(aes(y = log(RSS_Test), color = 'RSS of Test Set'), alpha = 0.5)
# show(plot.L1O)

# h) change test and train ratio
ratios <- seq(0.1, 0.9, 0.1)
models$Data <- list()
feature.columns <- c('V3', 'V4', 'V5', 'V7', 'V8')
shuffled.data <- data[sample(nrow(data)), c(feature.columns, 'V9')]
for (i in 1:length(ratios)) {
    ratio <- ratios[i]
    ratio.text <- sprintf('%.1f', ratio)
    models$Data[[ratio.text]] <- list()
    
    train <- shuffled.data[1:(nrow(data) * ratio),]
    test <- shuffled.data[(nrow(data) * ratio + 1):500,]
    
    models$Data[[ratio.text]] <- myLM.train(data.matrix(train[, feature.columns]),
                                            data.matrix(train[, c('V9')]),
                                            data.matrix(test[, feature.columns]),
                                            data.matrix(test[, c('V9')]))
}

df.ratio <- data.frame(x = c(1:9),
                       X = ratios,
                       L1O = c(
                           models$Data$`0.1`$L1O,
                           models$Data$`0.2`$L1O,
                           models$Data$`0.3`$L1O,
                           models$Data$`0.4`$L1O,
                           models$Data$`0.5`$L1O,
                           models$Data$`0.6`$L1O,
                           models$Data$`0.7`$L1O,
                           models$Data$`0.8`$L1O,
                           models$Data$`0.9`$L1O),
                       RSS_Train = c(
                           models$Data$`0.1`$train$rss,
                           models$Data$`0.2`$train$rss,
                           models$Data$`0.3`$train$rss,
                           models$Data$`0.4`$train$rss,
                           models$Data$`0.5`$train$rss,
                           models$Data$`0.6`$train$rss,
                           models$Data$`0.7`$train$rss,
                           models$Data$`0.8`$train$rss,
                           models$Data$`0.9`$train$rss),
                       RSS_Test = c(
                           models$Data$`0.1`$test$rss,
                           models$Data$`0.2`$test$rss,
                           models$Data$`0.3`$test$rss,
                           models$Data$`0.4`$test$rss,
                           models$Data$`0.5`$test$rss,
                           models$Data$`0.6`$test$rss,
                           models$Data$`0.7`$test$rss,
                           models$Data$`0.8`$test$rss,
                           models$Data$`0.9`$test$rss)
                       )
df.ratio$L1O_Mean <- df.ratio$L1O / (nrow(data) * ratios)
df.ratio$RSS_Train_Mean <- df.ratio$RSS_Train / (nrow(data) * ratios)
df.ratio$RSS_Test_Mean <- df.ratio$RSS_Test / (nrow(data) * ratios)

plot.ratio <- ggplot(data = df.ratio, aes(x = X))
plot.ratio <- plot.ratio + geom_line(aes(y = (L1O), color = 'Rcv L1O'))
plot.ratio <- plot.ratio + geom_point(aes(y = (L1O), color = 'Rcv L1O'), alpha = 0.5)
plot.ratio <- plot.ratio + geom_line(aes(y = (RSS_Train), color = 'RSS of Train Set'))
plot.ratio <- plot.ratio + geom_point(aes(y = (RSS_Train), color = 'RSS of Train Set'), alpha = 0.5)
plot.ratio <- plot.ratio + geom_line(aes(y = (RSS_Test), color = 'RSS of Test Set'))
plot.ratio <- plot.ratio + geom_point(aes(y = (RSS_Test), color = 'RSS of Test Set'), alpha = 0.5)
show(plot.ratio)

plot.ratio.mean <- ggplot(data = df.ratio, aes(x = X))
plot.ratio.mean <- plot.ratio.mean + geom_line(aes(y = (L1O_Mean), color = 'Rcv L1O'))
plot.ratio.mean <- plot.ratio.mean + geom_point(aes(y = (L1O_Mean), color = 'Rcv L1O'), alpha = 0.5)
plot.ratio.mean <- plot.ratio.mean + geom_line(aes(y = (RSS_Train_Mean), color = 'RSS of Train Set'))
plot.ratio.mean <- plot.ratio.mean + geom_point(aes(y = (RSS_Train_Mean), color = 'RSS of Train Set'), alpha = 0.5)
plot.ratio.mean <- plot.ratio.mean + geom_line(aes(y = (RSS_Test_Mean), color = 'RSS of Test Set'))
plot.ratio.mean <- plot.ratio.mean + geom_point(aes(y = (RSS_Test_Mean), color = 'RSS of Test Set'), alpha = 0.5)
show(plot.ratio.mean)

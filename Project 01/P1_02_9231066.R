if (!'ggplot2' %in% rownames(installed.packages())) {install.packages('ggplot2')}
library('ggplot2')
if (!'gridExtra' %in% rownames(installed.packages())) {install.packages('gridExtra')}
library('gridExtra')
if (!'reshape2' %in% rownames(installed.packages())) {install.packages('reshape2')}
library('reshape2')
if (!'dplyr' %in% rownames(installed.packages())) {install.packages('dplyr')}
library('dplyr')
if (!'tidyr' %in% rownames(installed.packages())) {install.packages('tidyr')}
library('tidyr')
if (!'mice' %in% rownames(installed.packages())) {install.packages('mice')}
library('mice')
if (!'glmnet' %in% rownames(installed.packages())) {install.packages('glmnet')}
library('glmnet')

# delete old data
rm(list = ls())
set.seed(9231066)

# read plot.data
data <- read.csv('data/dataset2.csv', header = FALSE)
extended <- read.csv('data/dataset2_extended.csv', header = FALSE)
test <- read.csv('data/dataset2_Unlabeled.csv', header = FALSE)


# a) scatter plots
plot.data <- data %>% gather(-V7, key = 'var', value = 'value')
    
scatter.plots <- list()
scatter.plots$all <- ggplot(plot.data, aes(x = value, y = V7))
scatter.plots$all <- scatter.plots$all + geom_point(color = '#56B4E9')
scatter.plots$all <- scatter.plots$all + geom_smooth(method = 'lm', color = 'red')
scatter.plots$all <- scatter.plots$all + facet_wrap(~ var, scales = 'free')
# show(scatter.plots$all)

scatter.plots$noZero <- plot.data %>%
    filter(value != 0) %>%
    ggplot(aes(x = value, y = V7)) +
    geom_point(color = '#56B4E9') +
    geom_smooth(method = 'lm', color = 'red') +
    facet_wrap(~ var, scales = 'free')
# show(scatter.plots$noZero)

# b) fill NAs
imputer <- list()
imputed <- list()
data[data == 0] <- NA
print(md.pattern(data))

imputer$train <- mice(data, defaultMethod = c('cart', 'norm.predict', 'rf', 'rf', 'cart', 'norm.nob'), m = 10)
# show(densityplot(imputer$train))
imputed$train <- complete(imputer$train)

plot.data <- imputed$train %>% gather(-V7, key = 'var', value = 'value')

scatter.plots$imputed <- ggplot(plot.data, aes(x = value, y = V7))
scatter.plots$imputed <- scatter.plots$imputed + geom_point(color = '#56B4E9')
scatter.plots$imputed <- scatter.plots$imputed + geom_smooth(method = 'lm', color = 'red')
scatter.plots$imputed <- scatter.plots$imputed + facet_wrap(~ var, scales = 'free')
# show(scatter.plots$imputed)

# c and d) lasso regression
### select train and test set
X <- model.matrix(V7 ~ ., imputed$train)[,-1]
y <- imputed$train$V7

### train lasso regressor
lasso.cost <- c()
x <- seq(-2, 0.5, length = 200)
for (i in x) {
    lambda <- 10^i
    lasso.mod <- glmnet(X[1:200, ], y[1:200], alpha = 1, lambda = lambda)
    lasso.pred <- predict(lasso.mod, X[1:200, ])
    lasso.cost <- c(lasso.cost, sum((lasso.pred - y[1:200]) ^ 2) + lambda * sum(abs(lasso.mod$beta)))
}
lasso.cost.plot <- ggplot(data.frame(x = x, cost = lasso.cost), aes(x = x, y = cost))
lasso.cost.plot <- lasso.cost.plot + geom_point(color = '#56B4E9')
# show(lasso.cost.plot)

lasso.best <- list()
lasso.best$lambda <- 10 ^ x[which.min(lasso.cost)]
lasso.best$model <- glmnet(X[1:200, ], y[1:200], alpha = 1, lambda = lasso.best$lambda)
lasso.best$train <- list()
lasso.best$train$pred <- predict(lasso.best$model, X[1:200, ])
lasso.best$train$residuals <- y[1:200] - lasso.best$train$pred
lasso.best$train$RSS <- sum(lasso.best$train$residuals ^ 2)
lasso.best$train$R2 <- 1 - lasso.best$train$RSS / sum((y[1:200] - mean(y[1:200])) ^ 2)
lasso.best$test <- list()
lasso.best$test$pred <- predict(lasso.best$model, X[201:240, ])
lasso.best$test$residuals <- y[201:240] - lasso.best$test$pred
lasso.best$test$RSS <- sum(lasso.best$test$residuals ^ 2)
lasso.best$test$R2 <- 1 - lasso.best$test$RSS / sum((y[201:240] - mean(y[201:240])) ^ 2)

print(sprintf('Lambda of best model: %f', lasso.best$lambda))
print(sprintf('   Train:'))
print(sprintf('      Residual Sum of Squares: %0.4f', lasso.best$train$RSS))
print(sprintf('      R Squared: %0.4f', lasso.best$train$R2))
print(sprintf('   Test:'))
print(sprintf('      Residual Sum of Squares: %0.4f', lasso.best$test$RSS))
print(sprintf('      R Squared: %0.4f', lasso.best$test$R2))

# e) prediction
# impute test data using completed train data
X <- imputed$train[, -7]
test[test == 0] <- NA
print(md.pattern(test))
X <- rbind(X, test)
print(md.pattern(X))
imputer$test <- mice(X, defaultMethod = c('cart', 'cart', 'pmm', 'cart', 'cart', 'midastouch'), m = 10)
densityplot(imputer$test)
imputed$test <- complete(imputer$test)
X <- imputed$test[1:240, ]

lasso.best$full.model <- glmnet(data.matrix(X), y, alpha = 1, lambda = lasso.best$lambda)
lasso.best$unlabeled <- list()
lasso.best$unlabeled$prediction <- predict(lasso.best$full.model, data.matrix(imputed$test[241:272, ]))

project.process <- cbind(test, lasso.best$unlabeled$prediction)
write.table(project.process, 'data/result.csv', sep = ',', row.names = FALSE, col.names = FALSE, na = '0')

# f) beta analysis
extended[extended == 0] <- NA
md.pattern(extended)

X <- model.matrix(V8 ~ ., extended)[,-1]
y <- extended$V8

lambda <- 0.001
model <- glmnet(X, y, alpha = 1, lambda = lambda)
extended.prediction <- predict(model, X)
print(sum((extended.prediction - y) ^ 2) + lambda * sum(abs(model$beta)))

print(model$a0)
print(model$beta)

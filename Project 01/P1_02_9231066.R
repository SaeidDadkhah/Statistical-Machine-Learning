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
data[data == 0] <- NA
md.pattern(data)

imputer <- mice(data)
imputed <- complete(imputer)
plot.data <- imputed %>% gather(-V7, key = 'var', value = 'value')

scatter.plots$imputed <- ggplot(plot.data, aes(x = value, y = V7))
scatter.plots$imputed <- scatter.plots$imputed + geom_point(color = '#56B4E9')
scatter.plots$imputed <- scatter.plots$imputed + geom_smooth(method = 'lm', color = 'red')
scatter.plots$imputed <- scatter.plots$imputed + facet_wrap(~ var, scales = 'free')
show(scatter.plots$imputed)

# c) lasso regression
### select train and test set
X <- model.matrix(V7 ~ ., imputed)[,-1]
y <- imputed$V7

### train lasso regressor
features <- c('V1', 'V2', 'V3', 'V4', 'V5', 'V6')
lasso.mod <- glmnet(X[1:200, ], y[1:200], alpha = 1, lambda = 0.01)
lasso.pred <- predict(lasso.mod, X[1:200, ])

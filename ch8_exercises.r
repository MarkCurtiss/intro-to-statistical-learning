# don't forget to upgrade R first!

# 7. In the lab, we applied random forests to the Boston data using mtry=6 and using ntree=25 and ntree=500.
#Create a plot displaying the test error resulting from random forests on this data set for a more comprehensive
#range of values for mtry and ntree. You can model your plot after Figure 8.10. Describe the results obtained.

install.packages('tree')
install.packages('ISLR')
install.packages('randomForest')
library(randomForest)
library(tree)
library(MASS)

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <- Boston[-train ,"medv"]

forest_results <- matrix(nrow=500, ncol=13)

for (mtries in 1:13) {
    message(Sys.time(), ": now working on m ", mtries)
    for (ntrees in 1:500) {
        if (ntrees %% 100 == 0) {
            cat('*')
        } else {
            cat('.')
        }
         rf.boston <- randomForest(medv~., data=Boston, subset=train, mtry=mtries, ntree=ntrees)
        yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
        forest_results[ntrees, mtries] = mean((yhat.rf - boston.test)^2)
    }
    message()
}


x <- seq(1,500)
y <- seq(min(forest_results), max(forest_results), length.out=500)
plot(x,y, xlab='Number of trees', ylab='Test set MSE', type='n')
for (i in 1:13) {
    lines(x, forest_results[,i], col=colors()[i])
}

labels <- sapply(seq(1,13), function(x) { paste("m=",x) })
colnames <- sapply(seq(1,13), function(x) { colors()[x] })
legend('topright', legend=labels, col=colnames, lty=1)

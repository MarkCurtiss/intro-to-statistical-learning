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
labels <- sapply(seq(1,13), function(x) { paste("m=",x) })
colnames <- sapply(seq(1,13), function(x) { colors()[x*10] })
legend('topright', legend=labels, col=colnames, lty=1)

for (i in 1:13) {
    color = colors()[i*10]
    lines(x, forest_results[,i], col=color)
    Sys.sleep(5)
}

# m = 3 seems to provide the lowest MSE.

#8. In the lab, a classification tree was applied to the Carseats data set after
#converting Sales into a qualitative response variable. Now we will seek to predict Sales
#using regression trees and related approaches, treating the response as a quantitative variable.
#(a) Split the data set into a training set and a test set.
library(ISLR)
set.seed(1)
carseats.train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
carseats.test <- Carseats[-train,]
sales.test <- Carseats[-train, 'Sales']
#(b) Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?
library(tree)
tree.carseats <- tree(Sales~.,Carseats, subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)
carseats.pred <- predict(tree.carseats, carseats.test)
carseat.mse <- mean((carseats.pred - sales.test)^2)

# The best sales are to be had when the shelf location is not bad and price is under 122.5
# I get a test MSE of 5.788809

#(c) Use cross-validation in order to determine the optimal level of tree complexity.
# Does pruning the tree improve the test MSE?
cv.carseats <- cv.tree(tree.carseats)
cv.carseats
plot(cv.carseats$size, cv.carseats$dev,type='b')
# the 12-node tree has the lowest deviance
prune.carseats <- prune.tree(tree.carseats, best=12)
plot(prune.carseats)
text(prune.carseats, pretty=0)
prune.pred <- predict(prune.carseats, newdata=carseats.test)
prune.mse <- mean((prune.pred - sales.test)^2)
prune.mse
# The test MSE is now 5.95124, which is worse!

#(d) Use the bagging approach in order to analyze this data. What test MSE do you obtain? Use the importance()
#function to determine which variables are most important.
bag.carseats <- randomForest(Sales~., data=Carseats, subset=carseats.train, mtry=10, importance=TRUE)
bag.pred <- predict(bag.carseats, newdata=carseats.test)
mean((bag.pred - sales.test)^2)
# I get a test MSE of 2.07669, a little over twice as good as my best single tree.
importance(bag.carseats)
varImpPlot(bag.carseats)
# Price and ShelveLoc are the most important variables.

# (e) Use random forests to analyze this data. What test MSE do you obtain? Use the importance() function
#to determine which variables are most important. Describe the effect of m, the number of variables
#considered at each split, on the error rate obtained.

carseat.error_rates <- rep(1,10)

for (i in 1:10) {
    rf.carseats <-  randomForest(Sales~., data=Carseats, subset=carseats.train, mtry=i, importance=TRUE)
    rf.pred <- predict(rf.carseats, newdata=carseats.test)
    carseat.error_rates[i] = mean((rf.pred - sales.test)^2)
}

carseat.error_rates
plot(seq(1,10), carseat.error_rates, type='b')
carseat.error_rates[8]
# Increases in m decrease error rate until m = 8.  m = 8 provides a test MSE of 2.072451
varImpPlot(rf.carseats <- randomForest(Sales~., data=Carseats, subset=carseats.train, mtry=8, importance=TRUE))
# Price and ShelveLoc are (still) the most important variables

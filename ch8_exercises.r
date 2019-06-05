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

# 9. This problem involves the OJ data set which is part of the ISLR package.
# (a) Create a training set containing a random sample of 800 observations, and a test set containing
# the remaining observations.

library(ISLR)
library(tree)
oj.train <- sample(800)
oj.test <- OJ[-oj.train,]

# (b) Fit a tree to the training data, with Purchase as the response and the other variables as predictors.
# Use the summary() function to produce summary statistics about the tree, and describe the results obtained.
# What is the training error rate? How many terminal nodes does the tree have?
tree.oj <- tree(Purchase~.,OJ, subset=oj.train)
summary(tree.oj)
# Variables actually used in tree construction:
# [1] "LoyalCH"   "PriceDiff"
# Number of terminal nodes:  7
# Residual mean deviance:  0.7342 = 582.3 / 793
# Misclassification error rate: 0.165 = 132 / 800
# 7 terminal nodes with a training error rate of .165

# (c) Type in the name of the tree object in order to get a detailed text output.
# Pick one of the terminal nodes, and interpret the information displayed.
tree.oj
     # 8) LoyalCH < 0.035047 62    0.00 MM ( 0.00000 1.00000 ) *
# If Loyalty to Citrus Hill is lower than 3% than 100% of customer are predicted to buy Minute Maid.

# (d) Create a plot of the tree, and interpret the results.
plot(tree.oj)
text(tree.oj, pretty=FALSE)
# Very loyal Citrus Hill customers won't buy Minute Maid even if there's a price difference.
# Moderately loyal Citrus Hill (between .26 and .5) will still buy Minute Maid if it is cheaper.
# Science reporting is hard!

# (e) Predict the response on the test data, and produce a confusion matrix comparing the test labels to the
# predicted test labels. What is the test error rate?
oj.predict <- predict(tree.oj, oj.test, type='class')
table(oj.predict, oj.test$Purchase)
# oj.predict  CH  MM
#         CH 141  44
#         MM  15  70
1 - (141+70)/(141+44+15+70)
# 21.8%

# (f) Apply the cv.tree() function to the training set in order to determine the optimal tree size.
cv.oj <- cv.tree(tree.oj, FUN=prune.misclass)
which.min(cv.oj$dev)
cv.oj$size[which.min(cv.oj$dev)]
cv.oj$size[which.min(cv.oj$dev)]
# 7

# (g) Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis.
plot(cv.oj$size, cv.oj$dev, type='b')
# (h) Which tree size corresponds to the lowest cross-validated classification error rate?
# 7

# (i) Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation.
# If cross-validation does not lead to selection of a pruned tree, then create a pruned tree with five terminal nodes.
prune.oj <- prune.tree(tree.oj, best=5)

# (j) Compare the training error rates between the pruned and unpruned trees. Which is higher?
summary(tree.oj)
summary(prune.oj)
# They are the same !

# (k) Compare the test error rates between the pruned and unpruned trees. Which is higher?
prune.predict <- predict(prune.oj, oj.test, type='class')
table(prune.predict, oj.test$Purchase)
# oj.predict  CH  MM
#         CH 141  44
#         MM  15  70
1 - (141+70)/(141+44+15+70)
# 21.8%
# They are the same !

# 10. We now use boosting to predict Salary in the Hitters data set.
# (a) Remove the observations for whom the salary information is unknown, and then log-transform the salaries.
library(ISLR)
library(gbm)

paid_hitters <- na.omit(Hitters)
paid_hitters$Salary = log(paid_hitters$Salary)

# (b) Create a training set consisting of the first 200 observations, and a test set consisting of the remaining
# observations.

hitters.train <- paid_hitters[1:200,]
hitters.test <- paid_hitters[201:263,]

# (c) Perform boosting on the training set with 1,000 trees for a range of values of the shrinkage parameter λ.
# Produce a plot with different shrinkage values on the x-axis and the corresponding training set MSE on the y-axis.
set.seed(1)
boost.training_error_rates <- rep(1,10)
shrinkages <- seq(0.001, 0.2, length.out=10)

for (i in 1:10) {
    boost.hitters <- gbm(Salary~., data=paid_hitters, distribution='gaussian', n.trees=1000, shrinkage=shrinkages[i])
    boost.training_error_rates[i] = mean(boost.hitters$train.error)
}

plot(shrinkages, boost.training_error_rates, type='b')

# (d) Produce a plot with different shrinkage values on the x-axis and the corresponding test set MSE on the y-axis.
set.seed(1)
boost.test_error_rates <- rep(1,10)
shrinkages <- seq(0.001, 0.2, length.out=10)

for (i in 1:10) {
    boost.hitters <- gbm(Salary~., data=paid_hitters, distribution='gaussian', n.trees=1000, shrinkage=shrinkages[i])
    pred.hitters <- predict(boost.hitters, newdata=hitters.test, n.trees=1000)
    boost.test_error_rates[i] = mean((pred.hitters - hitters.test$Salary)^2)
}

plot(shrinkages, boost.test_error_rates, type='b')

# (e) Compare the test MSE of boosting to the test MSE that results from applying two of the regression approaches
# seen in Chapters 3 and 6.
lm.hitters <- glm(Salary~., data=paid_hitters, subset=1:200)
pred.hitters <- predict(lm.hitters, newdata=hitters.test)
lm.error_rate <- mean((pred.hitters - hitters.test$Salary)^2)
lm.error_rate > boost.test_error_rates
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

install.packages('pls')
library(pls)
pcr.hitters <- pcr(Salary~., data=paid_hitters, validation='CV')
validationplot(pcr.hitters)
# M = 19
pcr.pred <- predict(pcr.hitters, hitters.test, ncomp=19)
pcr.error_rate <- mean((pcr.pred - hitters.test$Salary)^2)
pcr.error_rate > boost.test_error_rates
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# (f) Which variables appear to be the most important predictors in the boosted model?
varImpPlot(boost.hitters)
# This function only works for objects of class `randomForest'
summary(boost.hitters)
# CAtBat and Chits

# (g) Now apply bagging to the training set. What is the test set MSE for this approach?
library(randomForest)
bag.hitters <- randomForest(Salary~., data=paid_hitters, subset=1:200, mtry=19, importance=TRUE)
bag.pred <- predict(bag.hitters, newdata=hitters.train)
mean((bag.pred - hitters.train$Salary)^2)
# [1] 0.0335809
mean((bag.pred - hitters.train$Salary)^2) > boost.test_error_rates
# It's better !

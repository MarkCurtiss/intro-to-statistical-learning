## 5. We have seen that we can fit an SVM with a non-linear kernel in order to perform classification using a non-linear decision boundary. We will now see that we can also obtain a non-linear decision boundary by performing logistic regression using non-linear transformations of the features.
## (a) Generate a data set with n = 500 and p = 2, such that the observations belong to two classes with a quadratic decision boundary between them. For instance, you can do this as follows:
x1 <- runif(500)-0.5
x2 <- runif(500)-0.5
y <- 1*(x1^2-x2^2 > 0)
# (b) Plot the observations, colored according to their class labels. Your plot should display X1 on the x-axis, and X2 on the y- axis.
plot(x1, x2, col=c('red', 'blue'))

# (c) Fit a logistic regression model to the data, using X1 and X2 as predictors.
linear.model <- glm(y~x1+x2)

## (d) Apply this model to the training data in order to obtain a predicted class label for each training observation. Plot the observations, colored according to the predicted class labels. The decision boundary should be linear.
linear.predictions <- predict(linear.model, as.factor(y))



# 7. In this problem, you will use support vector approaches in order to predict whether a given car gets high or low gas mileage based on the Auto data set.
library(ISLR)
install.packages('e1071')
library(e1071)

# (a) Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 for cars with gas mileage below the median.

Auto$above_avg_mpg <- as.factor(ifelse(Auto$mpg > median(Auto$mpg), yes=1, no=0))

# (b) Fit a support vector classifier to the data with various values of cost, in order to predict whether
# a car gets high or low gas mileage. Report the cross-validation errors associated with different values
# of this parameter. Comment on your results.

linear.tune <- tune(svm, above_avg_mpg~.,
                    data=Auto,
                    kernel='linear',
                    ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(linear.tune)
# Cost = 1 seems to produce the best results.

# (c) Now repeat (b), this time using SVMs with radial and polynomial basis kernels,
# with different values of gamma and degree and cost. Comment on your results.
polynomial.svm <- tune(svm, above_avg_mpg~.,
                       data=Auto,
                       kernel='polynomial',
                       ranges=list(degree=c(1,2,3,4,5,6), cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

summary(polynomial.svm)
# - best parameters:
#  degree cost
#       1  100

radial.svm <- tune(svm, above_avg_mpg~.,
                   data=Auto,
                   kernel='radial',
                   ranges=list(gamma=c(0.1,2,3,4), cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(radial.svm)
# - best parameters:
#  gamma cost
# 0.1   10
# (d) Make some plots to back up your assertions in (b) and (c).
plot(polynomial.svm$best.model, Auto, mpg~weight)
polynomial.worst <- svm(above_avg_mpg~., data=Auto, kernel='polynomial', degree=1, cost=0.001)
plot(polynomial.worst, Auto, mpg~weight)

# polynomial.worst is nearly all support vectors

# 8. This problem involves the OJ data set which is part of the ISLR package.
# (a) Create a training set containing a random sample of 800 observations, and a test set containing
# the remaining observations.
library(ISLR)
set.seed(1)
train <- sample(800)
oj.train <- OJ[train,]
oj.test <- OJ[-train,]

#(b) Fit a support vector classifier to the training data using cost=0.01, with Purchase as the
# response and the other variables as predictors. Use the summary() function to produce summary
# statistics, and describe the results obtained.
oj.svc <- svm(Purchase~., data=oj.train, kernel='linear', cost=0.01)
summary(oj.svc)

## Call:
## svm(formula = Purchase ~ ., data = oj.train, kernel = "linear", cost = 0.01)


## Parameters:
##    SVM-Type:  C-classification
##  SVM-Kernel:  linear
##        cost:  0.01

## Number of Support Vectors:  424

##  ( 212 212 )


## Number of Classes:  2

## Levels:
##  CH MM

# (c) What are the training and test error rates?
svc.train.pred <- predict(oj.svc, oj.train)
table(svc.train.pred, oj.train$Purchase)
(448+228)/(448+228+75+49)
# [1] 0.845
svc.test.pred <- predict(oj.svc, oj.test)
table(svc.test.pred, oj.test$Purchase)
(133+81)/(133+81+33+23)
# [1] 0.7925926

# (d) Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10.
oj.svc.cv <- tune(svm, Purchase~., data=oj.train, kernel='linear',
                  ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(oj.svc.cv)
# - best parameters:
#  cost
#     1

# (e) Compute the training and test error rates using this new value for cost.
oj.svc.best <- svm(Purchase~., data=oj.train, kernel='linear', cost=1)

svc.best.pred <- predict(oj.svc.best, oj.train)
table(svc.best.pred, oj.train$Purchase)
(447+234)/(447+69+50+234)
# [1] 0.85125
svc.best.test.pred  <- predict(oj.svc.best, oj.test)
table(svc.best.test.pred, oj.test$Purchase)
(134+80)/(134+34+22+80)
# [1] 0.7925926

# The training error improved slightly but the test error doesn't appear to have changed

# (f) Repeat parts (b) through (e) using a support vector machine with a radial kernel. Use the default value for gamma.
oj.radial <- tune(svm, Purchase~., data=oj.train, kernel='radial',
                   ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))$best.model

radial.train.pred <- predict(oj.radial, oj.train)
table(radial.train.pred, oj.train$Purchase)

pct_of_correct_predictions <- function(predictions, actual) {
    confusion_matrix <- table(predictions, actual)
    (
        (confusion_matrix[1,1] + confusion_matrix[2,2]) / sum(confusion_matrix)
    ) * 100
}

pct_of_correct_predictions(radial.train.pred, oj.train$Purchase)
# [1] 85.125
radial.test.pred <- predict(oj.radial, oj.test)
pct_of_correct_predictions(radial.test.pred, oj.test$Purchase)
# [1] 81.85185

# (g) Repeat parts (b) through (e) using a support vector machine with a polynomial kernel. Set degree=2.
oj.polynomial <- tune(svm, Purchase~., data=oj.train, kernel='polynomial', degree=2,
                      ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))$best.model
polynomial.train.pred <- predict(oj.polynomial, oj.train)
pct_of_correct_predictions(polynomial.train.pred, oj.train$Purchase)
# [1] 84.5
polynomial.test.pred <- predict(oj.polynomial, oj.test)
pct_of_correct_predictions(polynomial.test.pred, oj.test$Purchase)
# [1] 81.48148

# (h) Overall, which approach seems to give the best results on this data?
# SVM with a radial kernel.

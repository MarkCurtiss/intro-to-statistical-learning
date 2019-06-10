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

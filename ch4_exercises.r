# 1.
# No!
# This is possible with algebra

# 2.
# No!

# 3.
# No!

# 4.
# Our group is not doing these.

# 5.
# a)
# We expect quadratic discrimant analysis to perform better on the training set and linear discrimant analysis
# to perform better on the test set.
# b)
# We expect quadratic discrimant analysis to perform better on the training and test sets.
# c)
# We expect the prediction accuracy of QDA relative to LDA to improve as it is more flexible.
# See also pg. 150: "In contrast, QDA is recommended if the training set is very large, so that
# the variance of the classifier is not a major concern..."
# d)
# False.  We may achieve a better training error rate using QDA but we can't assume a better test
# error rate, as QDA is likely to overfit the training data.

# 6.
receive_an_a <- function (hours_studied, undergrad_gpa) {
     exp(-6 + 0.05*hours_studied + 1*undergrad_gpa) /
     (1 + exp(-6 + 0.05*hours_studied + 1 * undergrad_gpa))
}
receive_an_a(40, 3.5)
# a) 37.7%

 receive_an_a(50, 3.5)
# b) 50 hours

# 7.
# computed by hand:  0.7518525

predicted_probability <- function (prior_yes, prior_no, current_val, variance, mean_yes, mean_no) {
    numerator <- prior_yes * exp((-1/(2*variance)) * (current_val - mean_yes)^2)
    denominator <- prior_no * exp((-1/(2*variance)) * (current_val - mean_no)^2) + numerator
    return(numerator/denominator)
}
predicted_probability(.8, .2, 4, 36, 10, 0)
# [1] 0.7518525

# 8.
# The KNN classifier will have a 0% training error rate and thus it has a 36% test error rate.
# This means we should use logistic regression with its 30% test error rate.


# 9.
.37 = (0.37)/(1-0.37)
# 0.5873016, or 37 in 63 will default. WRONG
.16/(1-.16)
# b) 0.1904762, or 4 in 21

# 10.
install.packages("ISLR")
require(ISLR)
plot(Weekly)
# a) Volume increases by year.  I don't see other obvious patterns.

log.weekly <- glm(Direction~.-Today-Year,family="binomial",data=Weekly)
# b) The Lag2 variable is statistically significant (p-value 3%) but not
# enough that I'd want to try and trade on it!

pct_of_correct_predictions <- function(confusion_matrix) {
    (
        (confusion_matrix[1,1] + confusion_matrix[2,2]) / sum(confusion_matrix)
    ) * 100
}

log.probs <- predict(log.weekly,type="response")
log.predictions <- rep("Down", dim(Weekly)[1])
log.predictions[log.probs > 0.5] = "Up"
confusion_matrix <- table(log.predictions, Weekly$Direction, dnn=c('predictions', 'actual'))
confusion_matrix
pct_of_correct_predictions(confusion_matrix)
mean(log.predictions==Weekly$Direction) * 100

# c)
#            Weekly
# predictions Down  Up
#        Down   54  48
#        Up    430 557
# 56.10652
# Logistic regression does better on days when it guesses 'Up', with a 56% success rate
# vs a 53% success rate on the days when it guesses 'Down'.

train <- (Weekly$Year < 2009)
training_data <- Weekly[train, ]
test_data <- Weekly[!train,]

log.weekly_trained <- glm(Direction~Lag2,family="binomial",data=Weekly,subset=train)
log.trained_probs <- predict(log.weekly_trained,test_data,type="response")
log.trained_predictions <- rep("Down", dim(test_data)[1])
log.trained_predictions[log.trained_probs > 0.5] = "Up"
confusion_matrix <- table(log.trained_predictions, test_data$Direction, dnn=c('predictions', 'actual'))
confusion_matrix
pct_of_correct_predictions(confusion_matrix)

# d)
#            actual
# predictions Down Up
#        Down    9  5
#        Up     34 56
# [1] 62.5

library(MASS)
lda.weekly_trained <- lda(Direction~Lag2,data=Weekly,subset=train)
lda.predictions <- predict(lda.weekly_trained, test_data)$class
confusion_matrix <- table(lda.predictions, test_data$Direction, dnn=c('predictions', 'actual'))
confusion_matrix
pct_of_correct_predictions(confusion_matrix)

# e)
#            actual
# predictions Down Up
#        Down    9  5
#        Up     34 56
# [1] 62.5

qda.weekly_trained <- qda(Direction~Lag2,data=Weekly,subset=train)
qda.predictions <- predict(qda.weekly_trained, test_data)$class
confusion_matrix <- table(qda.predictions, test_data$Direction, dnn=c('predictions', 'actual'))
confusion_matrix
pct_of_correct_predictions(confusion_matrix)

# f)
#            actual
# predictions Down Up
#        Down    0  0
#        Up     43 61
# [1] 58.65385

require(class)
train.X <- data.frame(Weekly[train,]$Lag2)
test.X <- data.frame(Weekly[!train,]$Lag2)
train.Direction <- Weekly[train,]$Direction
set.seed(1)
knn.predictions <- knn(train.X, test.X, train.Direction, k=1)
confusion_matrix <- table(knn.predictions, test_data$Direction, dnn=c('predictions', 'actual'))
confusion_matrix
pct_of_correct_predictions(confusion_matrix)
# g)
#            actual
# predictions Down Up
#        Down   21 30
#        Up     22 31
# [1] 50

# h) LDA and QDA performed the best with a 62.5% success rate.

assess_model_predictions <- function(model, test_data, predictor) {
    predictions <- predict(model, test_data)$class
    confusion_matrix <- table(predictions, test_data[[predictor]], dnn=c('predictions', 'actual'))
    confusion_matrix
    return(pct_of_correct_predictions(confusion_matrix))
}

assess_model_predictions(lda(Direction~Lag2+Today,data=Weekly,subset=train), test_data, 'Direction')
assess_model_predictions(qda(Direction~Lag2+Today,data=Weekly,subset=train), test_data, 'Direction')
# i) It's cheating, but I can get above 95% if I include 'Today'!
# [1] 96.15385.
# [1] 98.07692
# Seriously though.  I tried using knn at a wide variety of k and I also tried squaring, rooting, logging,
# and interacting various combinations of variables and I couldn't improve on lda(Direction~Lag2) or
# qda(Direction~Lag2).

# 11.
# a)
Auto <- read.csv("~/Documents/intro_to_statistical_learning/Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
mpg01 <- (Auto$mpg > median(Auto$mpg))
auto_with_mpg01 <- data.frame(Auto, mpg01)

pairs(auto_with_mpg01)
boxplot(auto_with_mpg01)
plot(auto_with_mpg01$mpg01, auto_with_mpg01$weight)
# b) Weight seems useful in predicting mpg01.  Horsepower also seems correlated although lots of low-horsepower
# cars also have below average mpg.  Same with displacement.

# c)
mpg_train <- Auto$year < 75
auto_train <- auto_with_mpg01[mpg_train,]
auto_test <- auto_with_mpg01[!mpg_train,]

# d)
library(MASS)
assess_model_predictions(
    lda(mpg01~weight+displacement+horsepower,data=auto_with_mpg01,subset=mpg_train),
    auto_test,
    "mpg01"
)
# [1] 89.2562, or a test error of 10.7438%

# e)
assess_model_predictions(
    qda(mpg01~weight+displacement,data=auto_with_mpg01,subset=mpg_train),
    auto_test,
    "mpg01"
 )
# [1] 87.19008, or a test error of 12.80992

# 12.
Power <- function() {
    print(2^3)
}
Power()
# a)
# [1] 8

Power2 <- function(x,a) {
    print(x^a)
}
Power2(3,8)
# b)
# [1] 6561

Power2(10,3)
Power2(8,17)
Power2(131,3)
# c)
# [1] 1000
# [1] 2.2518e+15
# [1] 2248091

Power3 <- function (x,a){
    return(x^a)
}
vals <- 1:10
plot(x=vals,y=Power3(vals,2), xlab='x', ylab='x^2', main="x vs x^2")
# e) I actually prefer the normal scale rather than logarithmic!

PlotPower <- function(x, a) {
    plot(x=x, y=Power3(x,a))
}
PlotPower(1:10, 2)

# 13.
high_crime <- (Boston$crim > median(Boston$crim))
Boston_with_high_crim <- data.frame(Boston, high_crime)

train <- rownames(Boston_with_high_crim) < 300
training_data <- Boston_with_high_crim[train,]
test_data <- Boston_with_high_crim[!train,]

assess_model_predictions(
    lda(high_crime~nox,data=Boston_with_high_crim,subset=train),
    test_data=test_data,
    "high_crime"
)
# [1] 86.17021

assess_model_predictions(
    qda(high_crime~nox,data=Boston_with_high_crim,subset=train),
    test_data=test_data,
    "high_crime"
)
# [1] 86.17021

log.boston_trained <- glm(high_crime~nox,family="binomial",data=Boston_with_high_crim,subset=train)
log.probs <- predict(log.boston_trained,type="response")
log.predictions <- rep(FALSE, dim(test_data)[1])
log.predictions[log.probs > 0.5] = TRUE
confusion_matrix <- table(log.predictions, test_data$high_crime, dnn=c('predictions', 'actual'))
confusion_matrix
pct_of_correct_predictions(confusion_matrix)
# [1] 36.52482

require(class)
train.X <- data.frame(Boston_with_high_crim[train,]$nox)
test.X <- data.frame(Boston_with_high_crim[!train,]$nox)
train.high_crime <- Boston_with_high_crim[train,]$high_crime
set.seed(1)
knn.predictions <- knn(train.X, test.X, train.high_crime, k=1)
confusion_matrix <- table(knn.predictions, test_data$high_crime, dnn=c('predictions', 'actual'))
confusion_matrix
pct_of_correct_predictions(confusion_matrix)

# [1] 83.33333

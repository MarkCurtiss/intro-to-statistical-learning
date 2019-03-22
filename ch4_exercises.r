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
# False.  We may achieve a better error training rate using QDA but we can't assume a better test
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
# TODO: Use the probability function

# 8.
# The KNN classifier will have a 0% training error rate and thus it had a 36% test error rate.
# This means we should use logistic regression with its 30% test error rate.


# 9.
# TODO: use the odds function
# a) 37/100 ?
# b) 4 in 25.

# 10.
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
# 62.5

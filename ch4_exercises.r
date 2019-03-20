# 1.
# No!

# 2.
# No!

# 3.
# No!

# 4.
# Our group is not doing these.

# 5.
# a)
# We expect linear discrimant analysis to perform better on the training and test sets.
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
# No!

# 8.
# Is this a trick question about averages ? I don't think we have enough information to decide since the
# KNN classifier could've had a training error rate of 0% but a test error rate of 36%, or a training
# error rate of 1% and a test error rate of 35%.

# 9.
# a)
# b) 4 in 25.

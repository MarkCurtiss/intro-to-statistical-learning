# 1.
# No!

# 2.
# a)
1 - 1/n

# b)
1 - 1/n

# c)
# The probability that it's not in the first event is 1 - 1/n, the probability that
# it's not in the second event is 1 - 1/n, so the probability of it not being in the
# first two events is (1 - 1/n)*(1 - 1/n), therefore the prbability it's not in the
# bootstrap is (1 - 1/n)^n

# d)
1 - (1 - 1/5)^5
# [1] 0.67232

# e)
# [1] 0.6339677

# f)
# [1] 0.632139

# g)
x <- c(1:100000)
plot(x, 1-(1-1/x)^x)

# h)
# store <- rep(NA, 100000)
# for (i in 1:100000) {
#   store[i] = sum(sample(1:100, rep=TRUE)==4) > 0
# }
# mean(store)
# [1] 0.63162

# 3.
# a)
# You take slice up your dataset into k slices.  For each slice,# you
# fit your model to the slice and then test it against the remainder
# of the data.  You average the MSE across all K fits.
# b)
# Advantages: It's computationally cheaper than LOOCV and it introduces less
# bias than the validation set approach
# Disadvantages: Compared to LOOCV you fit far fewer models, so you have higher
# variance.

# 4. No!

# 5.
# a)
set.seed(1)
library(ISLR)
train <- sample(10000, 1000)
lm.fit <- glm(default~income+balance,family="binomial",data=Default,subset=train)

# b)
log.probs <- predict(lm.fit,type="response")
log.predictions <- rep("No", length(log.probs))
log.predictions[log.probs > 0.5] = "Yes"
mean(log.predictions!=Default[-train,]$default) * 100
# [1] 4.422222

# c)
error_rates <- rep(3)
for (i in 1:3) {
  train <- sample(10000, 1000)
  lm.fit <- glm(default~income+balance,family="binomial",data=Default,subset=train)
  log.probs <- predict(lm.fit,type="response")
  log.predictions <- rep("No", length(log.probs))
  log.predictions[log.probs > 0.5] = "Yes"
  error_rates[i] <- mean(log.predictions!=Default[-train,]$default) * 100
}

error_rates
# I think this is an easy data set to predict, since nearly all values are "No"
# [1] 5.088889 4.377778 4.688889

# d)
error_rates <- rep(3)
for (i in 1:3) {
  train <- sample(10000, 1000)
  lm.fit <- glm(default~income+balance+student, family="binomial", data=Default, subset=train)
  log.probs <- predict(lm.fit,type="response")
  log.predictions <- rep("No", length(log.probs))
  log.predictions[log.probs > 0.5] = "Yes"
  error_rates[i] <- mean(log.predictions!=Default[-train,]$default) * 100
}
error_rates
# This doesn't appear to improve the error rate?
# [1] 5.266667 4.800000 4.333333

# 6.
set.seed(1)

# a)
library(ISLR)
train <- sample(10000, 1000)
lm.fit <- glm(default~income+balance,family="binomial",data=Default,subset=train)
coefficients(summary(lm.fit))
#                  Estimate   Std. Error    z value     Pr(>|z|)
# (Intercept) -1.178521e+01 1.444078e+00 -8.1610623 3.320908e-16
# income       1.335275e-05 1.720756e-05  0.7759816 4.377599e-01
# balance      6.011566e-03 7.879445e-04  7.6294277 2.357981e-14

# b)
boot.fn <- function(dataset, observations) {
    lm.fit <- glm(default~income+balance,family="binomial",data=dataset,subset=observations)
    coefficients(lm.fit)
}

# c)
library(boot)
boot(Default, boot.fn, R=1000)

# ORDINARY NONPARAMETRIC BOOTSTRAP
# Call:
# boot(data = Default, statistic = boot.fn, R = 1000)
# Bootstrap Statistics :
#          original        bias     std. error
# t1* -1.154047e+01 -4.952225e-02 4.136788e-01
# t2*  2.080898e-05  2.943832e-07 4.910237e-06
# t3*  5.647103e-03  2.144899e-05 2.175920e-04

# d)
# The std errors are pretty close!

# 1.
# No!

# 2.

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

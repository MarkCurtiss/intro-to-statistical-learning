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
# do I compare to Default$default or the training set?
100 - mean(log.predictions==Default[train,]$default) * 100
# if the training set, then my validation error is 2.7%
